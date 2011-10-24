
module Graphics.Bling.Renderer.PPM (

   ProgressivePhotonMap, mkProgressivePhotonMap
   
   ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.STRef
import qualified Data.Vector.Mutable as MV
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
import Graphics.Bling.Spectrum

data ProgressivePhotonMap = PPM Int Flt -- ^ #photons and initial radius

instance Printable ProgressivePhotonMap where
   prettyPrint (PPM _ _) = PP.vcat [
      PP.text "Progressive Photon Map" ]

mkProgressivePhotonMap :: Int -> Flt -> ProgressivePhotonMap
mkProgressivePhotonMap = PPM

data LocalStats = LS
   { lsR2      :: {-# UNPACK #-} ! Flt
   , lsN       :: {-# UNPACK #-} ! Flt
   , lsFlux    :: {-# UNPACK #-} ! Spectrum
   }

data HitPoint s = Hit
   { hpBsdf    :: {-# UNPACK #-} ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Flt, Flt)
   , hpW       :: {-# UNPACK #-} ! Vector
   , hpF       :: {-# UNPACK #-} ! Spectrum
   , hpStats   :: ! (STRef s LocalStats)
   }

alpha :: Flt
alpha = 0.7

-- | creates a new @HitPoint@ if the provided @Bsdf@ contains non-specular
--   components, or an empty list otherwise
mkHitPoint :: Flt -> Bsdf -> Vector -> Spectrum -> Sampled s [HitPoint s]
mkHitPoint r2 bsdf wo f
   | not (bsdfHasNonSpecular bsdf) = return $! []
   | otherwise = do
      pxpos <- cameraSample >>= \cs -> return (imageX cs, imageY cs)
      stats <- liftSampled $ newSTRef $ LS r2 0 black
      let
         result = Hit bsdf pxpos wo f stats
      seq result $ return $! [result]

escaped :: Ray -> Scene -> Spectrum
escaped ray s = V.sum $ V.map (`le` ray) (sceneLights s)

mkHitPoints :: Scene -> MImage s -> Flt -> R.Rand s [HitPoint s]
mkHitPoints scene img r2 = {-# SCC "mkHitPoints" #-}
   liftM concat $ forM (splitWindow $ imageWindow img) $ \w ->
      liftM concat $ sample (mkRandomSampler 1) w 0 0 $ do
         ray <- fireRay $ sceneCam scene
         
         (hps, ls) <- case scene `intersect` ray of
                           Just int -> nextV scene int (-(rayDir ray)) white r2 0 7
                           Nothing -> return $! ([], escaped ray scene)
                           
         (px, py) <- cameraSample >>= \cs -> return (imageX cs, imageY cs)
         liftSampled $ addContrib img (False, ImageSample px py (1, ls))
         return $! hps
   
nextV :: Scene -> Intersection -> Vector -> Spectrum
   -> Flt -> Int -> Int -> Sampled s ([HitPoint s], Spectrum)
nextV s int wo t r2 d md = {-# SCC "nextV" #-} do
   let
      bsdf = intBsdf int
      ls = intLe int wo
   
   -- trace rays for specular reflection and transmission
   (refl, lsr) <- cont (d+1) md s bsdf wo (mkBxdfType [Specular, Reflection]) t r2
   (trans, lst) <- cont (d+1) md s bsdf wo (mkBxdfType [Specular, Transmission]) t r2
   here <- mkHitPoint r2 bsdf wo t
   seq refl $ seq trans $ seq here $ return $! (here ++ refl ++ trans, t * (lsr + lst + ls))

cont :: Int -> Int -> Scene -> Bsdf -> Vector -> BxdfType -> Spectrum -> Flt -> Sampled s ([HitPoint s], Spectrum)
cont d md s bsdf wo tp t r2
   | d == md = return $! ([], black)
   | otherwise = do
      let
         (BsdfSample _ pdf f wi) = sampleBsdf' tp bsdf wo 0.5 (0.5, 0.5)
         ray = Ray p wi epsilon infinity
         p = bsdfShadingPoint bsdf
         n = bsdfShadingNormal bsdf
         t' = sScale (f * t) (wi `absDot` n / pdf)

      if pdf == 0 || isBlack f
         then return $! ([], black)
         else case s `intersect` ray of
                   Just int -> nextV s int (-wi) t' r2 d md
                   Nothing -> return $! ([], escaped ray s)

tracePhoton :: Scene -> SpatialHash s -> Sampled s ()
tracePhoton scene sh = {-# SCC "tracePhoton" #-} do
   ul <- rnd' 0
   ulo <- rnd2D' 0
   uld <- rnd2D' 1
   
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
       wo = normalize $ rayDir ray
       
   when ((pdf > 0) && not (isBlack li)) $
      nextVertex scene sh (-wo) (scene `intersect` ray) (sScale li (absDot nl wo / pdf)) 0

nextVertex :: Scene -> SpatialHash s -> Vector -> Maybe Intersection -> Spectrum -> Int -> Sampled s ()
nextVertex _ _ _ Nothing _ _ = return ()
nextVertex scene sh wi (Just int) li d = {-# SCC "nextVertex" #-} do
   
   -- add contribution for this photon hit
   let
      bsdf = intBsdf int
      p = bsdfShadingPoint bsdf
      n = bsdfShadingNormal bsdf
      
   when (bsdfHasNonSpecular bsdf) $ liftSampled $ hashLookup sh p n $ \hit -> do
      stats <- readSTRef $ hpStats hit
      let
         nn = lsN stats
         ratio = (nn + alpha) / (nn + 1)
         r2' = lsR2 stats * ratio
         f = evalBsdf True (hpBsdf hit) (hpW hit) wi
         flux' = sScale (lsFlux stats + (li * f)) ratio
                    
      writeSTRef (hpStats hit) $ LS r2' (nn + alpha) flux'
      
   -- follow the path
   ubc <- rnd' $ 1 + d * 2
   ubd <- rnd2D' $ 2 + d
   let
      (BsdfSample _ spdf f wo) = sampleAdjBsdf bsdf wi ubc ubd
      pcont = if d > 4 then 0.8 else 1
      li' = sScale (f * li) (absDot wo n / (spdf * pcont))
      ray = Ray p wo epsilon infinity
      
   unless (spdf == 0 || isBlack li') $
      rnd' (2 + d * 2) >>= \x -> unless (x > pcont) $
         nextVertex scene sh (-wo) (scene `intersect` ray) li' (d+1)

data SpatialHash s = SH
   { shBounds  :: {-# UNPACK #-} ! AABB
   , shEntries :: ! (V.Vector (V.Vector (HitPoint s)))
   , shScale   :: {-# UNPACK #-} ! Flt -- ^ 1 / bucket size
   }

hash :: (Int, Int, Int) -> Int
hash (x, y, z) = abs $ (x * 73856093) `xor` (y * 19349663) `xor` (z * 83492791)

hashLookup :: SpatialHash s -> Point -> Normal -> (HitPoint s -> ST s ()) -> ST s ()
hashLookup sh p n fun = {-# SCC "hashLookup" #-} do
   let
      Vector x y z = abs $ (p - aabbMin (shBounds sh)) * vpromote (shScale sh)
      idx = hash (floor x, floor y, floor z) `rem` V.length (shEntries sh)
   
   V.forM_ (shEntries sh V.! idx) $ \hit -> do
      stats <- readSTRef $ hpStats hit
      
      let
         hpn = bsdfShadingNormal $ hpBsdf hit
         v = bsdfShadingPoint (hpBsdf hit) - p

      when (n `dot` hpn > 0 && sqLen v <= lsR2 stats) $ fun hit
      
mkHash :: V.Vector (HitPoint s) -> ST s (SpatialHash s)
mkHash hits = {-# SCC "mkHash" #-} do
   r2 <- let
            go m hp = do
               stats <- readSTRef $ hpStats hp
               return $! max (lsR2 stats) m
         in V.foldM' go 0 hits
   
   let
      r = sqrt r2
      sc = 1 / (2 * r)
      cnt = V.length hits
      
      bounds = V.foldl' go emptyAABB hits where
         go b h = let p = bsdfShadingPoint $ hpBsdf h
                  in extendAABB b $ mkAABB (p - vpromote r) (p + vpromote r)
   
   v' <- MV.replicate cnt []
   V.forM_ hits $ \hp -> do
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
         
      forM_ [(x, y, z) | x <- xs, y <- ys, z <- zs] $ \pos ->
         let idx = hash pos `rem` cnt
         in MV.read v' idx >>= \old -> MV.write v' idx (hp : old)

   -- convert to an array of arrays
   v <- V.generateM (MV.length v') $ \i -> fmap V.fromList (MV.read v' i)
   
   return $ SH bounds v sc

instance Renderer ProgressivePhotonMap where
   render (PPM n' r) job report = {-# SCC "render" #-} do
      seed <- R.ioSeed
      let
         scene = jobScene job
         w = SampleWindow 0 0 0 0 -- just a hack, should split off camera sample generation
         d = 3 -- sample depth
         n1d = 2 * d + 1
         n2d = d + 2
         sn = max 1 $ ceiling $ sqrt (fromIntegral n' :: Float)
         n = sn * sn

      imgd <- stToIO $ thaw $ mkJobImage job
      hitPoints <- liftM V.fromList $ stToIO $ R.runWithSeed seed $ mkHitPoints scene imgd (r*r)
      img <- stToIO $ freeze imgd
      
      forM_ [1..] $ \passNum -> do
         currImg <- stToIO $ thaw img
         hitMap <- stToIO $ mkHash hitPoints
         pseed <- R.ioSeed
         _ <- stToIO $ R.runWithSeed pseed $
            sample (mkStratifiedSampler sn sn) w n1d n2d $ tracePhoton scene hitMap

         stToIO $ V.forM_ hitPoints $ \hp -> do
            stats <- readSTRef $ hpStats hp
            let
               r2 = lsR2 stats
               (px, py) = hpPixel hp
               
            addContrib currImg
               -- we can skip the 1 / pi factor because in accumulation above we
               -- have that factor implicitly
               (True, ImageSample px py (1 / (r2 * fromIntegral n), hpF hp * lsFlux stats))
            
         img' <- stToIO $ freeze currImg
         _ <- report $ PassDone passNum img' (1 / fromIntegral passNum)
         return ()
         
      return ()

