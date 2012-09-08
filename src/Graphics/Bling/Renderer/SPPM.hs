
{-# LANGUAGE UnboxedTuples #-}

module Graphics.Bling.Renderer.SPPM (

   SPPM, mkSPPM
   
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector as V
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
   { hpBsdf    :: {-# UNPACK #-} ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Float, Float)
   , hpStatIdx :: {-# UNPACK #-} ! Int -- index into the PixelStats
   , hpW       :: {-# UNPACK #-} ! Vector
   , hpF       :: ! Spectrum
   }

--------------------------------------------------------------------------------
-- Tracing Camera Rays for Hitpoint Creation
--------------------------------------------------------------------------------

escaped :: Ray -> Scene -> Spectrum
escaped ray s = V.sum $ V.map (`le` ray) (sceneLights s)

data CamState s = CS
   { csRay     :: ! Ray
   , csDepth   :: ! Int
   , csMaxDep  :: ! Int
   , csScene   :: ! Scene
   , csT       :: ! Spectrum -- throughput
   , csLs      :: ! Spectrum  -- accumulated flux towards camera
   , csHps     :: ! (GrowVec MV.MVector s HitPoint)
   , csPxStats :: ! (PixelStats s)
   }

traceCam :: CamState s -> Sampled s (CamState s)
traceCam cs
   | csDepth cs == csMaxDep cs = return cs
   | otherwise = do
      
      let
         ray = csRay cs
         scene = csScene cs
         t = csT cs
         pxs = csPxStats cs
         
      case scene `intersect` ray of
         Nothing  -> return $! cs { csLs = csLs cs + t * escaped ray scene }
         Just int -> do
            
            let
               wo = -(rayDir ray)
               bsdf = intBsdf int
               
            -- record a hitpoint here
            when (bsdfHasNonSpecular (intBsdf int)) $ do
               px <- cameraSample >>= \c -> return (imageX c, imageY c)
               let h = (Hit bsdf px (sIdx pxs px) wo t) in seq h (liftSampled $ gvAdd (csHps cs) h)
               
            -- determine outgoing ray
            bsdfC <- rnd
            bsdfD <- rnd2D
            
            let
               (BsdfSample _ pdf f wi) = sampleBsdf' (mkBxdfType [Specular, Reflection, Transmission]) bsdf wo bsdfC bsdfD
               ray' = Ray p wi (intEpsilon int) infinity
               p = bsdfShadingPoint bsdf
               n = bsdfShadingNormal bsdf
               t' = f * t -- sScale (f * t) (wi `absDot` n / pdf)
               ls' = csLs cs + t * intLe int (-wi)
               
            if pdf == 0 || isBlack f
               then return $ cs { csLs = ls' }
               else traceCam cs { csDepth = 1 + csDepth cs, csT = t', csLs = ls', csRay = ray' }
               
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
         liftSampled $ addContrib img (False, (px, py, WS 1 (csLs cs)))
         
   lift $ gvFreeze result
   
--------------------------------------------------------------------------------
-- Tracing Photons from the Light Sources and adding Image Contribution
--------------------------------------------------------------------------------

tracePhoton :: Scene -> Float -> SpatialHash -> MImage (ST s) -> PixelStats s -> Sampled s ()
tracePhoton scene alpha sh img ps = {-# SCC "tracePhoton" #-} do
   ul <- rnd' 0
   ulo <- rnd2D' 0
   uld <- rnd2D' 1
   
   let
      (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
      wi = -(rayDir ray)
      ls = sScale li (absDot nl wi / pdf)
      
   when ((pdf > 0) && not (isBlack li)) $
      nextVertex scene alpha sh wi (scene `intersect` ray) ls 0 img ps

nextVertex :: Scene -> Float -> SpatialHash -> Vector ->
   Maybe Intersection -> Spectrum -> Int ->
   MImage (ST s) -> PixelStats s -> Sampled s ()

nextVertex _ _ _ _ Nothing _ _ _ _ = return ()
nextVertex scene alpha sh wi (Just int) li d img ps = {-# SCC "nextVertex" #-} do

   -- add contribution for this photon hit
   let
      bsdf = intBsdf int
      p = bsdfShadingPoint bsdf
      n = bsdfShadingNormal bsdf

   when (bsdfHasNonSpecular bsdf) $ liftSampled $ hashLookup sh p n ps $ \hit -> {-# SCC "contrib" #-} do
      stats <- slup ps hit
      let
         nn = lsN stats
         ratio = (nn + alpha) / (nn + 1)
         r2 = lsR2 stats
         f = evalBsdf False (hpBsdf hit) (hpW hit) wi
         (px, py) = hpPixel hit

      addContrib img (True, (px, py, WS (1 / (r2 * pi)) (hpF hit * f * li)))
      sUpdate ps hit (r2 * ratio, nn + alpha)
      
   -- follow the path
   ubc <- rnd' $ 1 + d * 2
   ubd <- rnd2D' $ 2 + d
   let
      (BsdfSample _ spdf f wo) = sampleAdjBsdf bsdf wi ubc ubd
      pcont = if d > 4 then 0.7 else 1
      li' = sScale (f * li) (1 / pcont) -- (absDot wo n / (spdf * pcont))
      ray = Ray p wo (intEpsilon int) infinity

   unless (spdf == 0 || isBlack li') $
      rnd' (2 + d * 2) >>= \x -> unless (x > pcont) $
         nextVertex scene alpha sh (-wo) (scene `intersect` ray) li' (d+1) img ps

--------------------------------------------------------------------------------
-- Per-Pixel Accumulation Stats
--------------------------------------------------------------------------------

-- | the per-pixel accumulation statistics
type Stats = (Float, Float) -- (radius², #photons)

-- | extracts the number of collected photons from the @Stats@
lsN :: Stats -> Float
lsN = snd

-- | extracts the current search radius from the @Stats@
lsR2 :: Stats -> Float
lsR2 = fst

data PixelStats s = PS !(UMV.MVector s Stats) {-# UNPACK #-} !SampleWindow

mkPixelStats :: SampleWindow -> Float -> ST s (PixelStats s)
mkPixelStats wnd r2 = UMV.replicate (w * h) (r2, 0) >>= \ v-> return $! PS v wnd
   where (w, h) = (xEnd wnd - xStart wnd + 1, yEnd wnd - yStart wnd + 1)
   
sIdx :: PixelStats s -> (Float, Float) -> Int
{-# INLINE sIdx #-}
sIdx (PS _ wnd) (px, py) = w * (iy - yStart wnd) + (ix - xStart wnd) where
   (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
   (ix, iy) = (min (w-1) (floor px), min (h-1) (floor py))

slup :: PixelStats s -> HitPoint -> ST s Stats
{-# INLINE slup #-}
slup (PS v _) hit = UMV.unsafeRead v (hpStatIdx hit)

sUpdate :: PixelStats s -> HitPoint -> Stats -> ST s ()
{-# INLINE sUpdate #-}
sUpdate (PS v _) hit = UMV.unsafeWrite v (hpStatIdx hit)

--------------------------------------------------------------------------------
-- Spatial Hashing for the Hitpoints
--------------------------------------------------------------------------------

data SpatialHash = SH
   { shBounds  :: {-# UNPACK #-} ! AABB
   , shEntries :: ! (V.Vector (V.Vector HitPoint))
   , shScale   :: {-# UNPACK #-} ! Float -- ^ 1 / bucket size
   }

hash :: (Int, Int, Int) -> Int
{-# INLINE hash #-}
hash (x, y, z) = abs $ (x * 73856093) `xor` (y * 19349663) `xor` (z * 83492791)

hashLookup :: SpatialHash -> Point -> Normal -> PixelStats s -> (HitPoint -> ST s ()) -> ST s ()
hashLookup sh p n ps fun = {-# SCC "hashLookup" #-}
   let
      Vector x y z = abs $ (p - aabbMin (shBounds sh)) * vpromote (shScale sh)
      idx = hash (floor x, floor y, floor z) `rem` V.length (shEntries sh)
      hits = V.unsafeIndex (shEntries sh) idx
   in V.forM_ hits $ \hit -> do
      stats <- slup ps hit
      let
         hpn = bsdfShadingNormal $ hpBsdf hit
         v = bsdfShadingPoint (hpBsdf hit) - p
      when (n `dot` hpn > 0 && sqLen v <= lsR2 stats) $ {-# SCC "hlFun" #-} fun hit
      
mkHash :: V.Vector HitPoint -> PixelStats s -> ST s SpatialHash
mkHash hits ps = {-# SCC "mkHash" #-} do
   r2 <- let
            go m hp = slup ps hp >>= \stats -> return $! max (lsR2 stats) m
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
      stats <- slup ps hp
      let
         r2p = lsR2 stats
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
   v <- V.generateM (MV.length v') $ \i -> fmap V.fromList (MV.read v' i)

   return $ SH bounds v invSize

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

onePass :: Int -> RenderM Bool
onePass passNum = do
   sc <- asks envScene
   i <- asks envImg
   hitPoints <- mkHitPoints
   ps <- asks pxStats
   hitMap <- lift $ stToIO $ mkHash hitPoints ps
   pseed <- lift R.ioSeed
   n1d' <- asks n1d
   n2d' <- asks n2d
   sn' <- asks sn
   alpha <- asks rsAlpha
   _ <- lift $ stToIO $ R.runWithSeed pseed $
      runSample (mkStratifiedSampler sn' sn') (SampleWindow 0 0 0 0) n1d' n2d' $ tracePhoton sc alpha hitMap i ps
   
   img' <- lift $ stToIO $ fst <$> freeze i
   rep <- asks report
   lift $ rep $ PassDone passNum img' (1 / fromIntegral (passNum * (sn' * sn')))
   
sq :: Monad m => [m Bool] -> m ()
sq [] = return ()
sq (x:xs) = x >>= \c -> when c $ sq xs

--------------------------------------------------------------------------------
-- Main Rendering Loop
--------------------------------------------------------------------------------

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
               
