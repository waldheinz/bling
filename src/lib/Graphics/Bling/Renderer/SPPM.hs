
{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.Renderer.SPPM (

   SPPM, mkSPPM

   ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Parallel.Strategies
import           Data.Bits
import           Data.Function ( on )
import           Data.List ( foldl' )
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Conc (numCapabilities)
import qualified System.Random.MWC as MWC
import qualified Text.PrettyPrint as PP

import           Graphics.Bling.Camera
import           Graphics.Bling.Image
import           Graphics.Bling.Light (le)
import           Graphics.Bling.Primitive
import qualified Graphics.Bling.Random as R
import           Graphics.Bling.Reflection
import           Graphics.Bling.Rendering
import           Graphics.Bling.Sampling
import           Graphics.Bling.Scene
import           Graphics.Bling.Utils

-- | the Stochastic Progressive Photon Mapping Renderer
data SPPM = SPPM {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   -- #photons, max depth, initial radius and alpha

instance Printable SPPM where
   prettyPrint (SPPM {}) = PP.vcat [
      PP.text "Stochastic Progressive Photon Map" ]

-- | creates a SPPM renderer
mkSPPM
   :: Int   -- ^ the number of photons to emit per pass
   -> Int   -- ^ the maximum depth (path length for eye paths)
   -> Float -- ^ the initial search radius for collecting photons
   -> Float -- ^ the algorithm's alpha parameter, determining the shrinking of the radius, must be in (0..1)
   -> SPPM
mkSPPM = SPPM

data HitPoint = Hit
   { hpBsdf    :: ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Float, Float)
   , hpR2      :: {-# UNPACK #-} ! Float
   , hpW       :: ! Vector
   , hpF       :: ! Spectrum
   }

instance NFData HitPoint where
    rnf x = seq x ()

hitPosition :: HitPoint -> Point
hitPosition = bsdfShadingPoint . hpBsdf

--------------------------------------------------------------------------------
-- Tracing Camera Rays for Hitpoint Creation
--------------------------------------------------------------------------------

escaped :: Ray -> Scene -> Spectrum
escaped ray s = V.sum $ V.map (`le` ray) (sceneLights s)

data CamState s = CS
   { csRay     :: ! Ray
   , csDepth   :: ! Int
   , csMaxDep  :: {-# UNPACK #-} ! Int
   , csScene   :: {-# UNPACK #-} ! Scene
   , csT       :: ! Spectrum -- throughput
   , csLs      :: ! Spectrum  -- accumulated flux towards camera
   , csHps     :: ! (GrowVec MV.MVector s HitPoint)
   , csR2      :: {-# UNPACK #-} ! Float
   , csPixel   :: {-# UNPACK #-} ! (Float, Float)
   }

followCam :: BxdfProp -> Intersection -> Vector -> CamState s -> Sampled s (CamState s)
followCam prop int wo cs = do
   bsdfC <- rnd
   bsdfD <- rnd2D

   let
      bsdf = intBsdf int
      (BsdfSample _ pdf f wi) = sampleBsdf' (mkBxdfType [prop, Specular]) bsdf wo bsdfC bsdfD
      ray' = Ray p wi (intEpsilon int) infinity
      p = bsdfShadingPoint bsdf
      t' = f * csT cs

   if pdf == 0 || isBlack f
      then return $ cs
      else traceCam cs { csDepth = 1 + csDepth cs, csT = t', csRay = ray' }

traceCam :: CamState s -> Sampled s (CamState s)
traceCam cs
   | csDepth cs == csMaxDep cs = return cs
   | otherwise = do

      let
         ray = csRay cs
         scene = csScene cs
         t = csT cs

      case scene `scIntersect` ray of
         Nothing  -> return $! cs { csLs = csLs cs + t * escaped ray scene }
         Just int -> do

            let
               wo = -(rayDir ray)
               bsdf = intBsdf int
               ls = csT cs * intLe int (-wo)

            -- record a hitpoint here
            when (bsdfHasNonSpecular bsdf) $ do
               let h = (Hit bsdf (csPixel cs) (csR2 cs) wo t) in seq h (liftSampled $ gvAdd (csHps cs) h)

            csr <- followCam Reflection int wo cs
            cst <- followCam Transmission int wo cs

            return $! cs { csLs = csLs cs + csLs csr + csLs cst + ls }

mkHitPoints :: RenderM ([V.Vector HitPoint])
mkHitPoints = do
   sc <- asks envScene
   img <- asks envImg
   md <- asks envMaxD
   r2s' <- psR2 <$> asks rsPxStats
   r2s <- lift $ stToIO $ UV.freeze r2s'

   let
      rlup = (\p -> r2s UV.! sIdx (sampleExtent img) p)
      wnds = splitWindow $ sampleExtent img

      eval :: (MWC.Seed, SampleWindow) -> (NFBVector HitPoint, (Image, (Int, Int)))
      eval (seed, wnd) = runST $ R.runWithSeed seed $ do
         hps <- R.liftR gvNew
         i <- R.liftR $ mkImageTile img wnd

         runSample (mkRandomSampler 1) wnd 0 0 $ do
            ray <- fireRay $ sceneCam sc
            p@(px, py) <- cameraSample >>= \c -> return (imageX c, imageY c)
            cs <- traceCam $ CS ray 0 md sc white black hps (rlup p) p
            liftSampled $ addSample i px py (csLs cs)

         hps' <- R.liftR $ gvFreeze hps
         i' <- R.liftR $ freeze i
         return $! (mkNFBVector hps', i')

   seeds <- lift $ sequence $ (replicate $ length wnds) R.ioSeed

   forM (parMap rdeepseq eval $ zip seeds wnds) $ \(hps, tile) -> do
      lift $ stToIO $ addTile img tile
      return $! unNFBVector hps

--------------------------------------------------------------------------------
-- Tracing Photons from the Light Sources and adding Image Contribution
--------------------------------------------------------------------------------

data LightState s = LS
   {  lsScene     :: ! Scene
   ,  lsHash      :: ! SpatialHash
   ,  lsImage     :: ! (MImage (ST s))
   ,  lsCounts    :: ! (UMV.MVector s Int)   -- counts the # of photon hits
   ,  lsLi        :: ! Spectrum
   ,  lsDepth     :: ! Int
   ,  lsRay       :: ! Ray
   ,  lsSidx      :: ! StatsIndex
   }

tracePhoton :: Scene -> SpatialHash -> MImage (ST s) -> (UMV.MVector s Int) -> Sampled s ()
tracePhoton scene sh img cnt = {-# SCC "tracePhoton" #-} do
   ul <- rnd' 0
   ulo <- rnd2D' 0
   uld <- rnd2D' 1

   let
      (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
      wi = -(rayDir ray)
      ls = sScale li (absDot nl wi / pdf)
      st = LS scene sh img cnt ls 0 ray $ sIdx (sampleExtent img)

   when ((pdf > 0) && not (isBlack ls)) $ followPhoton st

followPhoton :: LightState s -> Sampled s ()
followPhoton s = do
   let
      ray = lsRay s
      wi = - (rayDir ray)
      scene = lsScene s
      d = lsDepth s
      li = lsLi s

   case scene `scIntersect` ray of
      Nothing  -> return ()
      Just int -> do
         let
            inc i = UMV.read cnt i >>= \old -> UMV.write cnt i (old + 1)
            bsdf = intBsdf int
            p = bsdfShadingPoint bsdf
            ng = bsdfNg bsdf
            cnt = lsCounts s
            sh = lsHash s

         -- add contribution for this photon hit
         when (bsdfHasNonSpecular bsdf) $ liftSampled $ hashLookup sh p $ \hit -> do
            let
               r2 = hpR2 hit
               f = evalBsdf False (hpBsdf hit) (hpW hit) wi
               (px, py) = hpPixel hit
               l = sScale (hpF hit * f * li) (1 / (absDot wi ng * r2 * pi))
               img = lsImage s

            splatSample img px py l
            inc (lsSidx s $ (px, py))

         -- follow the path
         ubc <- rnd' $ 1 + d * 2
         ubd <- rnd2D' $ 2 + d

         let
            (BsdfSample _ spdf f wo) = sampleAdjBsdf bsdf wi ubc ubd
            pcont = if d > 7 then 0.8 else 1
            li' = sScale (f * li) (1 / pcont) -- (absDot wo n / (spdf * pcont))
            ray' = Ray p wo (intEpsilon int) infinity

         unless (spdf == 0 || isBlack li') $
            rnd' (2 + d * 2) >>= \x -> unless (x > pcont) $
               followPhoton s { lsRay = ray', lsLi = li', lsDepth = d + 1 }

--------------------------------------------------------------------------------
-- Per-Pixel Accumulation Stats
--------------------------------------------------------------------------------

data PixelStats s = PS
   {   psR2  :: ! (UMV.MVector s Float)
   ,  _psN   :: ! (UMV.MVector s Float)
   ,  _psM   :: ! (UMV.MVector s Int)
   ,  _psWnd :: {-# UNPACK #-} ! SampleWindow
   }

mkPixelStats :: SampleWindow -> (UMV.MVector s Float) -> ST s (PixelStats s)
mkPixelStats wnd r2v = do
   nv <- UMV.replicate (w * h) 0
   mv <- UMV.replicate (w * h) 0
   return $! PS r2v nv mv wnd
   where (w, h) = (xEnd wnd - xStart wnd + 1, yEnd wnd - yStart wnd + 1)

mergeStats :: PixelStats s -> UV.Vector Int -> ST s ()
mergeStats (PS _ _ m _) m' = forM_ [0 .. UMV.length m - 1] $ \i -> do
   x' <- UMV.unsafeRead m i
   UMV.unsafeWrite m i (UV.unsafeIndex m' $ i + x')

type StatsIndex = (Float, Float) -> Int

sIdx :: SampleWindow -> StatsIndex
{-# INLINE sIdx #-}
sIdx wnd (px, py) = w * (iy - yStart wnd) + (ix - xStart wnd) where
   (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
   (ix, iy) = (min w (truncate px), min h (truncate py))

statsUpdate
   :: PixelStats s
   -> Float                -- ^ alpha
   -> ST s ()
statsUpdate (PS r2v nv mv _) a = do
   forM_ [0 .. (UMV.length r2v)] $ \i -> do
      m <- UMV.unsafeRead mv i

      when (m > 0) $ do
         r2 <- UMV.unsafeRead r2v i
         n <- UMV.unsafeRead nv i

         let
            n' = n + a * fromIntegral m
            ratio = n' / (n + fromIntegral m)
            r2' = r2 * ratio

         UMV.unsafeWrite r2v i r2'
         UMV.unsafeWrite nv i n'
         UMV.unsafeWrite mv i 0

--------------------------------------------------------------------------------
-- Spatial Hashing for the Hitpoints
--------------------------------------------------------------------------------

data SpatialHash = SH
   { shBounds  :: {-# UNPACK #-} ! AABB
   , shEntries :: ! (V.Vector KdTree)
   , shScale   :: {-# UNPACK #-} ! Float -- ^ 1 / bucket size
   }

hash :: (Int, Int, Int) -> Int
{-# INLINE hash #-}
hash (x, y, z) = abs $ (x * 73856093) `xor` (y * 19349663) `xor` (z * 83492791)

hashLookup :: SpatialHash -> Point -> (HitPoint -> ST s ()) -> ST s ()
hashLookup sh p fun = {-# SCC "hashLookup" #-}
   let
      Vector x y z = abs $ (p - aabbMin (shBounds sh)) * vpromote (shScale sh)
      cnt = V.length (shEntries sh)
      idx =  max 0 $ min (cnt - 1) $ hash (truncate x, truncate y, truncate z) `rem` cnt
      tree = V.unsafeIndex (shEntries sh) idx
   in treeLookup tree p fun

mkHash :: [V.Vector HitPoint] -> ST s SpatialHash
mkHash hitlist = {-# SCC "mkHash" #-} do
   let
      r2 = maximum $ map (\hits -> V.foldl' (\m hp -> max (hpR2 hp) m) 0 hits) hitlist
      r = sqrt r2
      pm f = withStrategy (parBuffer numCapabilities rdeepseq) . map f
      cnt = sum $ map (\hits -> V.length hits) hitlist
      invSize = 1 / (2 * r)
      bounds = foldl' mappend mempty $ map (\hits -> V.foldl' go mempty hits) hitlist where
         go b h = let p = bsdfShadingPoint $ hpBsdf h
                  in mappend b $ mkAABB (p - vpromote r) (p + vpromote r)

   v' <- MV.replicate cnt []
   forM_ hitlist $ \hits -> V.forM_ hits $ \hp -> do
      let
         r2p = hpR2 hp
         rp = sqrt r2p
         pmin = aabbMin bounds

         p = bsdfShadingPoint $ hpBsdf hp
         Vector x0 y0 z0 = invSize *# (abs $ p - vpromote rp - pmin)
         Vector x1 y1 z1 = invSize *# (abs $ p + vpromote rp - pmin)
         xs = [truncate x0 .. truncate x1]
         ys = [truncate y0 .. truncate y1]
         zs = [truncate z0 .. truncate z1]

      unless (r2p == 0) $ forM_ [(x, y, z) | x <- xs, y <- ys, z <- zs] $ \i ->
         let idx = max 0 $ min (cnt - 1) $ hash i `rem` cnt
         in MV.unsafeRead v' idx >>= \o -> MV.unsafeWrite v' idx (hp : o)

   v'' <- V.freeze v'
   let v = V.fromList $ pm (\hpl -> mkKdTree (V.fromList hpl)) $ V.toList v''

   return $ SH bounds v invSize

--------------------------------------------------------------------------------
-- KdTree for hitpoint lookup inside hash cells
--------------------------------------------------------------------------------

data KdTree
   = Node {-# UNPACK #-} !Float !HitPoint !KdTree !KdTree
      -- max. radius² in subtree, hit, left, right
   | Leaf {-# UNPACK #-} !(NFBVector HitPoint)

instance NFData KdTree where
    rnf x = seq x ()

mkKdTree :: V.Vector HitPoint -> KdTree
mkKdTree hits = {-# SCC "mkKdTree" #-}
      runST $ V.thaw hits >>= \hits' -> liftM fst $ go 0 hits' where

   go depth v
      | MV.length v <= 5 = do
         v' <- V.freeze v

         return $! (
            Leaf $ mkNFBVector v',
            sqrt $ V.foldl' (\m hp -> max m $ hpR2 hp) 0 v')

      | otherwise = do
         let
            median = MV.length v `quot` 2
            axis = depth `rem` 3
            comp = compare `on` (\x -> hitPosition x .! axis)

         I.selectBy comp v median
         I.selectByBounds comp v 1 median (MV.length v)

         pivot <- MV.unsafeRead v median
         (left, lr)  <- go (depth + 1) $ MV.take median v
         (right, rr) <- go (depth + 1) $ MV.drop (median+1) v

         let mr = max (hpR2 pivot) $ max lr rr in
            return $! (Node mr pivot left right, mr)

treeLookup :: KdTree -> Point -> (HitPoint -> ST s ()) -> ST s ()
treeLookup t p fun = {-# SCC "treeLookup" #-} go 0 t where
   go _ (Leaf hps) = V.forM_ (unNFBVector hps) $ \hp ->
      when (sqLen (hitPosition hp - p) <= hpR2 hp) $ {-# SCC "leaf.fun" #-} fun hp

   go depth (Node mr hp l r) = do
      let
         axis = depth `rem` 3
         split = hitPosition hp .! axis
         pos = p .! axis

      when (sqLen (hitPosition hp - p) <= hpR2 hp) $ {-# SCC "node.fun" #-} fun hp
      when (pos - mr <= split) $ go (depth + 1) l
      when (pos + mr >= split) $ go (depth + 1) r

--------------------------------------------------------------------------------
-- Main Rendering Loop
--------------------------------------------------------------------------------

data RenderState s = RS
   { envImg    :: ! (MImage s)
   , n1d       :: ! Int
   , n2d       :: ! Int
   , envScene  :: ! Scene
   , rsPxStats :: ! (PixelStats (PrimState s))
   , sn        :: ! Int
   , envMaxD   :: ! Int
   , report    :: ! ProgressReporter
   , rsAlpha   :: ! Float
   }

type RenderM a = ReaderT (RenderState (ST RealWorld)) IO a

onePass :: Int -> RenderM Bool
onePass passNum = do
   sc <- asks envScene
   i <- asks envImg
   hitPoints <- mkHitPoints
   ps <- asks rsPxStats
   alpha <- asks rsAlpha
   hitMap <- lift $ stToIO $ mkHash hitPoints
   n1d' <- asks n1d
   n2d' <- asks n2d
   sn' <- asks sn

   let
      eval :: MWC.Seed -> (NFUVector Int, (Image, (Int, Int)))
      eval seed = runST $ do
         eimg <- splitMImage i
         eps <- UMV.replicate (windowPixels $ sampleExtent i) 0
         R.runWithSeed seed $
            runSample (mkStratifiedSampler sn' sn') (SampleWindow 0 0 0 0) n1d' n2d' $
            tracePhoton sc hitMap eimg eps

         fs' <- mkNFUVector <$> UV.freeze eps
         eimg' <- freeze eimg
         return $! (fs', eimg')

   seeds <- lift $ sequence $ (replicate numCapabilities) R.ioSeed

   forM_ (parMap rdeepseq eval seeds) $ \(ms, img) -> do
      lift $ stToIO $ addTile i img
      lift $ stToIO $ mergeStats ps (unNFUVector ms)

   lift $ stToIO $ statsUpdate ps alpha

   img' <- lift $ stToIO $ fst <$> freeze i
   rep <- asks report

   lift $ rep $ PassDone passNum img' (1 / fromIntegral (numCapabilities * passNum * (sn' * sn')))

sq :: Monad m => [m Bool] -> m ()
sq [] = return ()
sq (x:xs) = x >>= \c -> when c $ sq xs

instance Renderer SPPM where
   render (SPPM n md r alpha) job rep = {-# SCC "render" #-} do

      let
         scene = jobScene job
         d = 3 -- sample depth
         n1 = 2 * d + 1
         n2 = d + 2
         sn' = max 1 $ ceiling $ sqrt ((fromIntegral n :: Float) / fromIntegral numCapabilities)

      img <- stToIO $ thaw $ mkJobImage job
      r2s <- stToIO $ UMV.replicate (windowPixels (sampleExtent img)) (r * r)
      ps <- stToIO $ mkPixelStats (sampleExtent img) r2s

      sq $ map (\p -> runReaderT (onePass p) (RS img n1 n2 scene ps sn' md rep alpha)) [1..]
