
{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.Renderer.SPPM (

   SPPM, mkSPPM
   
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.Bits
import Data.Function (on)
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Light (le)
import Graphics.Bling.Primitive
import qualified Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Utils

import Debug.Trace

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
   , hpStatIdx :: {-# UNPACK #-} ! Int -- index into the PixelStats
   , hpW       :: ! Vector
   , hpF       :: ! Spectrum
   }

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
   , csPxStats :: {-# UNPACK #-} ! (PixelStats s)
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
         pxs = csPxStats cs
         
      case scene `scIntersect` ray of
         Nothing  -> return $! cs { csLs = csLs cs + t * escaped ray scene }
         Just int -> do
            
            let
               wo = -(rayDir ray)
               bsdf = intBsdf int
               ls = csT cs * intLe int (-wo)
               
            -- record a hitpoint here
            when (bsdfHasNonSpecular bsdf) $ do
               px <- cameraSample >>= \c -> return (imageX c, imageY c)
               let h = (Hit bsdf px (sIdx pxs px) wo t) in seq h (liftSampled $ gvAdd (csHps cs) h)
            
            csr <- followCam Reflection int wo cs
            cst <- followCam Transmission int wo cs   
            
            return $! cs { csLs = csLs cs + csLs csr + csLs cst + ls }
            
mkHitPoints :: RenderM (V.Vector HitPoint)
mkHitPoints = do
   sc <- asks envScene
   img <- asks envImg
   md <- asks envMaxD
   pxs <- asks pxStats
   result <- lift gvNew
   
   lift $ R.runRandIO $ forM_ (splitWindow $ sampleExtent img) $ \w ->
      runSample (mkRandomSampler 1) w 0 0 $ do
         ray <- fireRay $ sceneCam sc
         cs <- traceCam $ CS ray 0 md sc white black result pxs
         (px, py) <- cameraSample >>= \c -> return (imageX c, imageY c)
         liftSampled $ addSample img px py (csLs cs)
         
   lift $ gvFreeze result
   
--------------------------------------------------------------------------------
-- Tracing Photons from the Light Sources and adding Image Contribution
--------------------------------------------------------------------------------

data LightState s = LS
   {  lsScene     :: ! Scene
   ,  lsHash      :: ! SpatialHash
   ,  lsImage     :: ! (MImage (ST s))
   ,  lsStats     :: ! (PixelStats s)
   ,  lsLi        :: ! Spectrum
   ,  lsDepth     :: ! Int
   ,  lsRay       :: ! Ray
   }

tracePhoton :: Scene -> SpatialHash -> MImage (ST s) -> PixelStats s -> Sampled s ()
tracePhoton scene sh img ps = {-# SCC "tracePhoton" #-} do
   ul <- rnd' 0
   ulo <- rnd2D' 0
   uld <- rnd2D' 1
   
   let
      (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
      wi = -(rayDir ray)
      ls = sScale li (absDot nl wi / pdf)
      st = LS scene sh img ps ls 0 ray
      
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
            bsdf = intBsdf int
            p = bsdfShadingPoint bsdf
            ng = bsdfNg bsdf
            ps = lsStats s
            sh = lsHash s
            
         -- add contribution for this photon hit
         when (bsdfHasNonSpecular bsdf) $ liftSampled $ hashLookup sh p ps $ \hit -> do
--            stats <- slup ps hit
            r2 <- sr2 ps hit
            let
--               nn = lsN stats
--               ratio = (nn + alpha) / (nn + 1)
--               r2 = sr2 ps hit
               f = evalBsdf True (hpBsdf hit) (hpW hit) wi
               (px, py) = hpPixel hit
               l = sScale (hpF hit * f * li) (1 / (absDot wi ng * r2 * pi))
               img = lsImage s
               
            splatSample img px py l
            sRecordHit ps hit -- (r2 * ratio, nn + alpha)
            
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
   {  psR2  :: ! (UMV.MVector s Float)
   ,  psN   :: ! (UMV.MVector s Float)
   ,  psM   :: ! (UMV.MVector s Int)
   ,  psWnd :: {-# UNPACK #-} ! SampleWindow
   }

mkPixelStats :: SampleWindow -> Float -> ST s (PixelStats s)
mkPixelStats wnd r2 = do
   r2v <- UMV.replicate (w * h) r2
   nv <- UMV.replicate (w * h) 0
   mv <- UMV.replicate (w * h) 0
   return $! PS r2v nv mv wnd
   where (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
   
type FrozenStats = (UV.Vector Float, UV.Vector Float, UV.Vector Int, SampleWindow)
   
freezeStats :: PixelStats s -> ST s FrozenStats
freezeStats (PS r2 n m w) = do
   r2' <- UV.freeze r2
   n' <- UV.freeze n
   m' <- UV.freeze m
   return $! (r2', n', m', w)
   
thawStats :: FrozenStats -> ST s (PixelStats s)
thawStats (r2, n, m, w) = do
   r2' <- UV.thaw r2
   n' <- UV.thaw n
   m' <- UV.thaw m
   return $! PS r2' n' m' w
   
mergeStats :: PixelStats s -> UV.Vector Int -> ST s ()
mergeStats (PS _ _ m _) m' = forM_ [0 .. UMV.length m - 1] $ \i -> do
   x' <- UMV.unsafeRead m i
   UMV.unsafeWrite m i (m' UV.! i + x')
   
sIdx :: PixelStats s -> (Float, Float) -> Int
{-# INLINE sIdx #-}
sIdx (PS _ _ _ wnd) (px, py) = w * (iy - yStart wnd) + (ix - xStart wnd) where
   (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
   (ix, iy) = (min (w-1) (floor px), min (h-1) (floor py))

sr2 :: PixelStats s -> HitPoint -> ST s Float
{-# INLINE sr2 #-}
sr2 (PS r2 _ _ _) hp = UMV.unsafeRead r2 (hpStatIdx hp)

sRecordHit :: PixelStats s -> HitPoint -> ST s ()
{-# INLINE sRecordHit #-}
sRecordHit (PS _ _ m _) hit = do
   old <- UMV.unsafeRead m (hpStatIdx hit)
   UMV.unsafeWrite m (hpStatIdx hit) (old + 1)

statsUpdate
   :: PixelStats s
   -> Float                -- ^ alpha
   -> ST s ()
statsUpdate (PS r2v nv mv _) a = do
--   UV.generateM (UMV.length r2v) $ \i -> do
   forM_ [0 .. (UMV.length r2v)] $ \i -> do
      m <- UMV.unsafeRead mv i
      
      if (m > 0)
         then do
            r2 <- UMV.unsafeRead r2v i
            n <- UMV.unsafeRead nv i
         
            let
               n' = n + a * fromIntegral m
               ratio = n' / (n + fromIntegral m)
               r2' = r2 * ratio
         
            UMV.unsafeWrite r2v i r2'
            UMV.unsafeWrite nv i n'
            UMV.unsafeWrite mv i 0
            return $ r2' / r2
            
         else return 1
      
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

hashLookup :: SpatialHash -> Point -> PixelStats s -> (HitPoint -> ST s ()) -> ST s ()
hashLookup sh p ps fun = {-# SCC "hashLookup" #-}
   let
      Vector x y z = abs $ (p - aabbMin (shBounds sh)) * vpromote (shScale sh)
      cnt = V.length (shEntries sh)
      idx =  max 0 $ min (cnt - 1) $ hash (floor x, floor y, floor z) `rem` cnt
      tree = V.unsafeIndex (shEntries sh) idx
   in treeLookup tree p ps fun
      
mkHash :: V.Vector HitPoint -> PixelStats s -> ST s SpatialHash
mkHash hits ps = {-# SCC "mkHash" #-} do
   r2 <- let
            go m hp = sr2 ps hp >>= \r2 -> return $! max r2 m
         in V.foldM' go 0 hits
   
   let
      r = sqrt r2
      cnt = V.length hits
      invSize = 1 / (2 * r)
      bounds = V.foldl' go emptyAABB hits where
         go b h = let p = bsdfShadingPoint $ hpBsdf h
                  in extendAABB b $ mkAABB (p - vpromote r) (p + vpromote r)
   
   v' <- MV.replicate cnt []
   V.forM_ hits $ \hp -> do
      r2p <- sr2 ps hp
      
      let
         rp = sqrt r2p
         pmin = aabbMin bounds
         
         p = bsdfShadingPoint $ hpBsdf hp
         Vector x0 y0 z0 = abs $ (p - vpromote rp - pmin) * vpromote invSize
         Vector x1 y1 z1 = abs $ (p + vpromote rp - pmin) * vpromote invSize
         xs = [floor x0 .. floor x1]
         ys = [floor y0 .. floor y1]
         zs = [floor z0 .. floor z1]
         
      unless (r2p == 0) $ forM_ [(x, y, z) | x <- xs, y <- ys, z <- zs] $ \i ->
         let idx = max 0 $ min (cnt - 1) $ hash i `rem` cnt
         in MV.read v' idx >>= \o -> MV.write v' idx (hp : o)

   -- convert to an (non-mutable) array of arrays
   v <- V.generateM (MV.length v') $ \i -> do
      hps <- MV.read v' i
      x <- V.thaw $ V.fromList hps
      mkKdTree ps x

   return $ SH bounds v invSize

--------------------------------------------------------------------------------
-- KdTree for hitpoint lookup inside hash cells
--------------------------------------------------------------------------------

data KdTree
   = Node {-# UNPACK #-} !Float !HitPoint !KdTree !KdTree
      -- max. radius² in subtree, hit, left, right
   | Leaf !(V.Vector HitPoint)
      
mkKdTree :: PixelStats s -> MV.MVector (PrimState (ST s)) HitPoint -> ST s KdTree
mkKdTree pxs hits = {-# SCC "mkKdTree" #-} liftM fst $ go 0 hits where
   go depth v
      | MV.length v <= 5 = do
         v' <- V.freeze v
         mr2 <- V.foldM' (\m hp -> max m <$> sr2 pxs hp) 0 v'
         return $! ( Leaf v', sqrt mr2 )
         
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
         r <- sqrt <$> sr2 pxs pivot
      
         let mr = max r $ max lr rr in
            return $! (Node mr pivot left right, mr)
      
treeLookup :: KdTree -> Point -> PixelStats s -> (HitPoint -> ST s ()) -> ST s ()
treeLookup t p pxs fun = {-# SCC "treeLookup" #-} go 0 t where
   go _ (Leaf hps) = V.forM_ hps $ \hp -> do
      r2 <- sr2 pxs hp
      when (sqLen (hitPosition hp - p) <= r2) $ {-# SCC "leaf.fun" #-} fun hp
      
   go depth (Node mr hp l r) = do
      let
         axis = depth `rem` 3
         split = hitPosition hp .! axis
         pos = p .! axis
   
      r2 <- sr2 pxs hp
      when (sqLen (hitPosition hp - p) <= r2) $ {-# SCC "node.fun" #-} fun hp
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
   , pxStats   :: ! (PixelStats (PrimState s))
   , sn        :: ! Int
   , envMaxD   :: ! Int
   , report    :: ! ProgressReporter
   , rsAlpha   :: ! Float
   }

type RenderM a = ReaderT (RenderState (ST RealWorld)) IO a

npar :: Int
npar = 4

instance NFData (UV.Vector a) where

onePass :: Int -> RenderM Bool
onePass passNum = do
   sc <- asks envScene
   i <- asks envImg
   hitPoints <- mkHitPoints
   ps <- asks pxStats
   alpha <- asks rsAlpha
   hitMap <- lift $ stToIO $ mkHash hitPoints ps
   n1d' <- asks n1d
   n2d' <- asks n2d
   sn' <- asks sn
   
   let
      pm f = withStrategy (parBuffer npar rdeepseq) . map f
   
      eval :: (MWC.Seed, FrozenStats) -> (FrozenStats, (Image, (Int, Int)))
      eval (seed, fs) = runST $ do
         eimg <- splitMImage i
         eps <- thawStats fs
         R.runWithSeed seed $
            runSample (mkStratifiedSampler sn' sn') (SampleWindow 0 0 0 0) n1d' n2d' $
            tracePhoton sc hitMap eimg eps
   
         fs' <- freezeStats eps
         eimg' <- freeze eimg
         return $! (fs', eimg')

   seeds <- lift $ sequence $ (replicate npar) R.ioSeed
   fs <- lift $ stToIO $ freezeStats ps

   forM_ (pm eval $ zip seeds (repeat fs)) $ \((_, _, ms, _), img) -> do
      lift $ stToIO $ addTile i img
      lift $ stToIO $ mergeStats ps ms
      
--   lift $ stToIO $ R.runWithSeed pseed $
--      runSample (mkStratifiedSampler sn' sn') (SampleWindow 0 0 0 0) n1d' n2d' $ tracePhoton sc hitMap i ps
   
   lift $ stToIO $ statsUpdate ps alpha
   
   img' <- lift $ stToIO $ fst <$> freeze i
   rep <- asks report

   lift $ rep $ PassDone passNum img' (1 / fromIntegral (npar * passNum * (sn' * sn')))
   
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
         sn' = max 1 $ ceiling $ sqrt (fromIntegral n :: Float)

      img <- stToIO $ thaw $ mkJobImage job
      ps <- stToIO $ mkPixelStats (sampleExtent img) (r * r)
      
      sq $ map (\p -> runReaderT (onePass p) (RS img n1 n2 scene ps sn' md rep alpha)) [1..]
               
