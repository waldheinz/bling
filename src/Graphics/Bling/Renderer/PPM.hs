
module Graphics.Bling.Renderer.PPM (

   ProgressivePhotonMap, mkProgressivePhotonMap
   
   ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.List (foldl')
import Data.STRef
import qualified Data.Vector.Mutable as MV
import Debug.Trace
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Primitive
import qualified Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data ProgressivePhotonMap = PPM

instance Printable ProgressivePhotonMap where
   prettyPrint (PPM) = PP.vcat [
      PP.text "Progressive Photon Map" ]

mkProgressivePhotonMap :: ProgressivePhotonMap
mkProgressivePhotonMap = PPM

data LocalStats = LS
   { lsR2      :: {-# UNPACK #-} ! Flt
   , lsN       :: {-# UNPACK #-} ! Int
   , lsFlux    :: {-# UNPACK #-} ! Spectrum
   }

data HitPoint s = Hit
   { hpBsdf    :: {-# UNPACK #-} ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Flt, Flt)
   , hpW       :: {-# UNPACK #-} ! Vector
   , hpStats   :: ! (STRef s LocalStats)
   }

alpha :: Flt
alpha = 0.7

mkHitPoints :: Scene -> Image -> Flt -> R.Rand s [HitPoint s]
mkHitPoints scene img r2 = do
   let
      cam = sceneCam scene

   result <- R.newRandRef []
      
   forM_ (splitWindow $ imageWindow' img) $ \w -> do
      sample (mkRandomSampler 1) w 0 0 $ do
         p <- fireRay cam >>= traceRay scene r2
         liftSampled $ modifySTRef result (p++)

   R.readRandRef result

traceRay :: Scene -> Flt -> Ray -> Sampled s [HitPoint s]
traceRay scene r2 ray = maybe (return $! []) ls (scene `intersect` ray) where
   ls int = do
--      ubc <- rnd
--      ubd <- rnd2D
      let
         bsdf = intBsdf int
--          p = bsdfShadingPoint bsdf
--          n = bsdfShadingNormal bsdf
         wo = -(rayDir ray)

      -- trace rays for specular reflection and transmission
--      refl <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Reflection]
--      trans <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Transmission]
      pxpos <- do
         cs <- cameraSample
         return (imageX cs, imageY cs)

      stats <- liftSampled $ newSTRef $ LS r2 0 black
      return $! [Hit bsdf pxpos wo stats]

tracePhoton :: Scene -> SpatialHash s -> MImage s -> R.Rand s ()
tracePhoton scene sh img = do
   ul <- R.rnd
   ulo <- R.rnd2D
   uld <- R.rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
       wo = normalize $ rayDir ray
       splat = addContrib img
   if (pdf > 0) && not (isBlack li)
      then nextVertex scene sh (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0 splat
      else return ()

nextVertex :: Scene -> SpatialHash s -> Vector -> Maybe Intersection -> Spectrum -> Int -> ((Bool, ImageSample) -> ST s ()) -> R.Rand s ()
nextVertex _ _ _ Nothing _ _ _ = return ()
nextVertex scene sh wi (Just int) li d splat = do
   
   -- add contribution for this photon hit
   let
      bsdf = intBsdf int
      p = bsdfShadingPoint bsdf
      n = bsdfShadingNormal bsdf
      
   R.liftR $ hashLookup sh p n $ \hit -> do
      stats <- readSTRef $ hpStats hit
      let
         hn = lsN stats
         fn = fromIntegral hn
         g = (fn * alpha + alpha) / (fn * alpha + 1)
         r2' = lsR2 stats * g
         hpbsdf = hpBsdf hit
         f = evalBsdf hpbsdf (hpW hit) wi
         flux' = sScale (lsFlux stats + f * li) g
      writeSTRef (hpStats hit) (LS r2' (hn+1) flux')
      
   -- follow the path
   ubc <- R.rnd
   ubd <- R.rnd2D
   let
      (BsdfSample _ spdf f wo) = sampleBsdf bsdf wi ubc ubd
      pcont = if d > 3 then 0.5 else 1
      li' = sScale (f * li) (absDot wo n / (spdf * pcont))
      ray = Ray p wo epsilon infinity
      
   if (spdf == 0) || isBlack li'
      then return ()
      else R.rnd >>= \x -> if x > pcont
         then return ()
         else nextVertex scene sh (-wo) (scene `intersect` ray) li' (d+1) splat

data SpatialHash s = SH
   { shBounds  :: {-# UNPACK #-} ! AABB
   , shEntries :: ! (MV.MVector s [HitPoint s])
   , shScale   :: {-# UNPACK #-} ! Flt -- ^ bucket size
   }

hash :: (Int, Int, Int) -> Int
hash (x, y, z) = abs $ (x * 73856093) `xor` (y * 19349663) `xor` (z * 83492791)

hashLookup :: SpatialHash s -> Point -> Normal -> (HitPoint s -> ST s ()) -> ST s ()
hashLookup sh p n fun = do
   let
      Vector x y z = abs $ (p - (aabbMin $ shBounds sh)) * vpromote (shScale sh)
      idx = hash (floor x, floor y, floor z) `rem` MV.length (shEntries sh)

   hps <- MV.read (shEntries sh) idx
   forM_ hps $ \hit -> do
      stats <- readSTRef $ hpStats hit
      
      let
         hpn = bsdfShadingNormal $ hpBsdf hit
         v = (bsdfShadingPoint $ hpBsdf hit) - p

      if n `dot` hpn > 0 && sqLen v <= lsR2 stats
         then fun hit
         else return ()
   
mkHash :: [HitPoint s] -> ST s (SpatialHash s)
mkHash hits = do
   r <- fmap maximum $ forM hits $ \hp -> do
      stats <- readSTRef $ hpStats hp
      return $ sqrt $ lsR2 stats
      
   let
      sc = 1 / (2 * r)
      cnt = length hits
      bounds = foldl' go emptyAABB hits where
         go b h = let p = bsdfShadingPoint $ hpBsdf h
                  in extendAABB b $ mkAABB (p - vpromote r) (p + vpromote r)
   
   v <- MV.replicate cnt []
   forM_ hits $ \hp -> do
      stats <- readSTRef $ hpStats hp
      
      let
         p = bsdfShadingPoint $ hpBsdf hp
         rp = sqrt $ lsR2 stats
         pmin = aabbMin bounds
         Vector x0 y0 z0 = abs $ (p - vpromote rp - pmin) * vpromote sc
         Vector x1 y1 z1 = abs $ (p + vpromote rp - pmin) * vpromote sc
         xs = [floor x0 .. floor x1]
         ys = [floor y0 .. floor y1]
         zs = [floor z0 .. floor z1]
         
      forM_ [(x, y, z) | x <- xs, y <- ys, z <- zs] $ \pos -> do
         let idx = hash pos `rem` cnt
         old <- MV.read v idx
         MV.write v idx (hp : old)
   
   return $ SH bounds v sc

instance Renderer ProgressivePhotonMap where
   render (PPM) job report = do
      seed <- R.ioSeed
      let
         scene = jobScene job
         img = mkJobImage job
         r = 40
      
      hitPoints <- stToIO $ R.runWithSeed seed $ mkHitPoints scene img (r*r)
      
      forM_ [1..] $ \passNum -> do
         currImg <- stToIO $ thaw img
         hitMap <- stToIO $ mkHash hitPoints
         pseed <- R.ioSeed
         stToIO $ R.runWithSeed pseed $
            replicateM_ 30000 $ tracePhoton scene hitMap $ currImg

         stToIO $ forM_ hitPoints $ \hp -> do
            stats <- readSTRef $ hpStats hp
            let
               r2 = lsR2 stats
               (px, py) = hpPixel hp
               
            addContrib currImg $
               (True, ImageSample px py (1 / (r2 * 30000), lsFlux stats))
            
         img' <- stToIO $ freeze currImg
         _ <- report $ PassDone passNum img' (1 / fromIntegral passNum)
         return ()
         
      return ()

