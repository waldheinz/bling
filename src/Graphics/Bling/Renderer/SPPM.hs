
module Graphics.Bling.Renderer.SPPM (

   SPPM, mkSPPM
   
   ) where

import Control.Applicative
import Control.Monad
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

-- | the Stochastic Progressive Photon Mapping Renderer
data SPPM = SPPM {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Float -- ^ #photons and initial radius

instance Printable SPPM where
   prettyPrint (SPPM _ _ _) = PP.vcat [
      PP.text "Stochastic Progressive Photon Map" ]

-- | creates a SPPM renderer
mkSPPM
   :: Int -- ^ the number of photons to emit per pass
   -> Int -- ^ the maximum depth (path length for eye paths)
   -> Float -- ^ the initial search radius for collecting photons
   -> SPPM
mkSPPM = SPPM

data HitPoint = Hit
   { hpBsdf    :: {-# UNPACK #-} ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Float, Float)
   , hpW       :: {-# UNPACK #-} ! Vector
   , hpF       :: {-# UNPACK #-} ! Spectrum
   }

-- | the algorithm's alpha parameter, determining the ratio of new photons
--   to keep in each pass
alpha :: Float
alpha = 0.7

--------------------------------------------------------------------------------
-- Tracing Camera Rays for Hitpoint Creation
--------------------------------------------------------------------------------

-- | creates a new @HitPoint@ if the provided @Bsdf@ contains non-specular
--   components, or an empty list otherwise
mkHitPoint :: Bsdf -> Vector -> Spectrum -> Sampled s [HitPoint]
mkHitPoint bsdf wo f
   | not (bsdfHasNonSpecular bsdf) = return $! []
   | otherwise = do
      pxpos <- cameraSample >>= \cs -> return (imageX cs, imageY cs)
      return $! [Hit bsdf pxpos wo f]

escaped :: Ray -> Scene -> Spectrum
escaped ray s = V.sum $ V.map (`le` ray) (sceneLights s)

mkHitPoints :: Scene -> MImage (ST s) -> Int -> R.Rand s [HitPoint]
mkHitPoints scene img maxD = {-# SCC "mkHitPoints" #-}
   liftM concat $ forM (splitWindow $ sampleExtent img) $ \w ->
      liftM concat $ sample (mkRandomSampler 1) w 0 0 $ do
         ray <- fireRay $ sceneCam scene
         
         (hps, ls) <- case scene `intersect` ray of
            Just int -> let wo = (-(rayDir ray))
                            ls = intLe int wo
                        in do
                           (hs, ls') <- nextV scene int wo white 0 maxD
                           return $! (hs, ls' + ls)
            Nothing  -> return $! ([], escaped ray scene)
                           
         (px, py) <- cameraSample >>= \cs -> return (imageX cs, imageY cs)
         liftSampled $ addContrib img (False, (px, py, WS 1 ls))
         return $! hps
   
nextV :: Scene -> Intersection -> Vector -> Spectrum
   -> Int -> Int -> Sampled s ([HitPoint], Spectrum)
nextV s int wo t d md = {-# SCC "nextV" #-} do
   let
      bsdf = intBsdf int
      e = intEpsilon int
   
   -- trace rays for specular reflection and transmission
   (re, lsr) <- cont e (d+1) md s bsdf wo (mkBxdfType [Specular, Reflection]) t
   (tr, lst) <- cont e (d+1) md s bsdf wo (mkBxdfType [Specular, Transmission]) t
   here <- mkHitPoint bsdf wo t
   seq re $ seq tr $ seq here $ return $! (here ++ re ++ tr, lsr + lst)

cont :: Float -> Int -> Int -> Scene -> Bsdf -> Vector -> BxdfType -> Spectrum -> Sampled s ([HitPoint], Spectrum)
cont eps d md s bsdf wo tp t
   | d == md = return $! ([], black)
   | otherwise = do
      bsdfC <- rnd
      bsdfD <- rnd2D
      
      let
         (BsdfSample _ pdf f wi) = sampleAdjBsdf' tp bsdf wo bsdfC bsdfD
         ray = Ray p wi eps infinity
         p = bsdfShadingPoint bsdf
         n = bsdfShadingNormal bsdf
         t' = sScale (f * t) (wi `absDot` n / pdf)
      if pdf == 0 || isBlack f
         then return $! ([], black)
         else case s `intersect` ray of
            Just int -> do
               (hs, ls') <- nextV s int (-wi) t' d md
               return $! (hs, ls' + (t' * intLe int (-wi)))
            Nothing  -> return $! ([], t' * escaped ray s)

--------------------------------------------------------------------------------
-- Tracing Photons from the Light Sources and adding Image Contribution
--------------------------------------------------------------------------------

tracePhoton :: Scene -> SpatialHash -> MImage (ST s) -> PixelStats s -> Sampled s ()
tracePhoton scene sh img ps = {-# SCC "tracePhoton" #-} do
   ul <- rnd' 0
   ulo <- rnd2D' 0
   uld <- rnd2D' 1
   
   let
      (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
      wi = -(rayDir ray)
      ls = sScale li (absDot nl wi / pdf)
      
   when ((pdf > 0) && not (isBlack li)) $
      nextVertex scene sh wi (scene `intersect` ray) ls 0 img ps

nextVertex :: Scene -> SpatialHash -> Vector ->
   Maybe Intersection -> Spectrum -> Int ->
   MImage (ST s) -> PixelStats s -> Sampled s ()

nextVertex _ _ _ Nothing _ _ _ _ = return ()
nextVertex scene sh wi (Just int) li d img ps = {-# SCC "nextVertex" #-} do

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
         r2' = r2 * ratio
         f = evalBsdf True (hpBsdf hit) (hpW hit) wi
         nn' = nn + alpha
         (px, py) = hpPixel hit

      addContrib img (True, (px, py, WS (1 / (r2 * pi)) (hpF hit * f * li)))
      sUpdate ps hit (r2', nn')

   -- follow the path
   ubc <- rnd' $ 1 + d * 2
   ubd <- rnd2D' $ 2 + d
   let
      (BsdfSample _ spdf f wo) = sampleBsdf bsdf wi ubc ubd
      pcont = if d > 4 then 0.8 else 1
      li' = sScale (f * li) (absDot wo n / (spdf * pcont))
      ray = Ray p wo (intEpsilon int) infinity

   unless (spdf == 0 || isBlack li') $
      rnd' (2 + d * 2) >>= \x -> unless (x > pcont) $
         nextVertex scene sh (-wo) (scene `intersect` ray) li' (d+1) img ps

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

data PixelStats s = PS (UMV.MVector s Stats) SampleWindow

mkPixelStats :: SampleWindow -> Float -> ST s (PixelStats s)
mkPixelStats wnd r2 = do
   v <- UMV.replicate ((w + 1) * (h + 1)) (r2, 0)
   return $! PS v wnd
   where
      (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
      
sIdx :: PixelStats s -> HitPoint -> Int
{-# INLINE sIdx #-}
sIdx (PS _ wnd) hit = w * (iy - yStart wnd) + (ix - xStart wnd) where
   (w, h) = (xEnd wnd - xStart wnd, yEnd wnd - yStart wnd)
   (px, py) = hpPixel hit
   (ix, iy) = (min (w-1) (floor px), min (h-1) (floor py))

slup :: PixelStats s -> HitPoint -> ST s Stats
{-# INLINE slup #-}
slup ps@(PS v _) hit = UMV.unsafeRead v (sIdx ps hit)

sUpdate :: PixelStats s -> HitPoint -> Stats -> ST s ()
{-# INLINE sUpdate #-}
sUpdate ps@(PS v _) hit = UMV.unsafeWrite v (sIdx ps hit)

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

--------------------------------------------------------------------------------
-- Main Rendering Loop
--------------------------------------------------------------------------------

instance Renderer SPPM where
   render (SPPM n' maxD r) job report = {-# SCC "render" #-} do
      
      let
         scene = jobScene job
         w = SampleWindow 0 0 0 0 -- just a hack, should split off camera sample generation
         d = 3 -- sample depth
         n1d = 2 * d + 1
         n2d = d + 2
         sn = max 1 $ ceiling $ sqrt (fromIntegral n' :: Float)
         n = sn * sn

      img <- stToIO $ thaw $ mkJobImage job
      pxStats <- stToIO $ mkPixelStats (sampleExtent img) (r * r)
      
      forM_ [1..] $ \passNum -> do
         seed <- R.ioSeed
         hitPoints <- liftM V.fromList $ stToIO $ R.runWithSeed seed $ mkHitPoints scene img maxD
         hitMap <- stToIO $ mkHash hitPoints pxStats
         pseed <- R.ioSeed
         _ <- stToIO $ R.runWithSeed pseed $
            sample (mkStratifiedSampler sn sn) w n1d n2d $ tracePhoton scene hitMap img pxStats
         
         img' <- stToIO $ fst <$> freeze img
         _ <- report $ PassDone passNum img' (1 / fromIntegral (passNum * n))
         return ()
         
