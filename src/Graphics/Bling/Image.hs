
module Graphics.Bling.Image (
   module Graphics.Bling.Filter,
   
   Image, ImageSample(..), mkImage, rgbPixels, imageWindow', imgW, imgH, imgFilter,
   
   MImage, imageFilter,
   
   mkMImage, mkImageTile, addSample, splatSample, addContrib, addTile,
   
   imageWidth, imageHeight, sampleExtent, writePpm, 
   
   thaw, freeze,

   -- * Reading and Writing RGBE

   writeRgbe
   
   ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Debug.Trace
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import System.IO

import Graphics.Bling.Filter
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum

-- | an image pixel, which consists of the sample weight, the sample RGB value
--   and the RGB value of the splatted samples
type Pixel = (Float, (Float, Float, Float), (Float, Float, Float))

emptyPixel :: Pixel
emptyPixel = (0, (0, 0, 0), (0, 0, 0))

-- | a mutable image
data MImage m = MImage
   { imageWidth   :: {-# UNPACK #-} ! Int
   , imageHeight  :: {-# UNPACK #-} ! Int
   , _imageOffset :: {-# UNPACK #-} ! (Int, Int)
   , imageFilter  :: ! Filter
   , _imagePixels :: MV.MVector (PrimState m) Pixel
   }

-- | creates a new image where all pixels are initialized to black
mkMImage :: (PrimMonad m, Functor m)
   => Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> m (MImage m)
mkMImage flt w h = MImage w h (0, 0) flt <$> (MV.replicate (w * h) emptyPixel)

mkImageTile :: (PrimMonad m, Functor m) => Filter -> SampleWindow -> m (MImage m)
mkImageTile f wnd = MImage w h (px, py) f <$> pixels where
   w = xEnd wnd - px + floor (0.5 + fw)
   h = yEnd wnd - py + floor (0.5 + fh)
   px = max 0 $ xStart wnd -- + floor (0.5 - fw)
   py = max 0 $ yStart wnd -- + floor (0.5 - fh)
   pixels = MV.replicate (w * h) emptyPixel
   (fw, fh) = filterSize f
   
-- | an immutable image
data Image = Img
   { imgW      :: {-# UNPACK #-} ! Int
   , imgH      :: {-# UNPACK #-} ! Int
   , imgFilter :: ! Filter
   , _imgP     :: V.Vector Pixel
   }

instance NFData Image where
   rnf (Img w h f p) = w `seq` h `seq` f `seq` p `seq` ()

instance Show Image where
   show (Img w h f _) = "Image {width=" ++ (show w) ++ ", height=" ++ (show h) ++ ", filter=" ++ (show f) ++ "}"

-- | creates a new image where all pixels are initialized to black
mkImage
   :: Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> Image
mkImage flt w h = Img w h flt $ V.replicate (w * h) emptyPixel

-- | converts an image to a mutable image 
thaw :: (PrimMonad m, Functor m) => Image -> m (MImage m)
thaw (Img w h f p) = {-# SCC "thaw" #-} MImage w h (0, 0) f <$> GV.thaw p

-- | converts a mutable image to an image (and the offsets)
freeze :: (PrimMonad m, Functor m) => MImage m -> m (Image, (Int, Int))
freeze (MImage w h o f p) = {-# SCC "freeze" #-} (\p' -> (Img w h f p', o)) <$> GV.freeze p

sampleExtent :: PrimMonad m => MImage m -> SampleWindow
sampleExtent (MImage w h (ox, oy) f _) = SampleWindow x0 x1 y0 y1 where
   x0 = ox + floor (0.5 - filterWidth f)
   x1 = ox + floor (0.5 + (fromIntegral w) + filterWidth f)
   y0 = oy + floor (0.5 - filterHeight f)
   y1 = oy + floor (0.5 + (fromIntegral h) + filterHeight f)

imageWindow' :: Image -> SampleWindow
imageWindow' (Img w h f _) = SampleWindow x0 x1 y0 y1 where
   x0 = floor (0.5 - filterWidth f)
   x1 = floor (0.5 + (fromIntegral w) + filterWidth f)
   y0 = floor (0.5 - filterHeight f)
   y1 = floor (0.5 + (fromIntegral h) + filterHeight f)

addTile :: PrimMonad m => MImage m -> (Image, (Int, Int)) -> m ()
addTile (MImage w h (ox, oy) _ px) (Img tw th _ px', (dx, dy)) = do
   forM_ [(x, y) | y <- [0 .. (th-1)], x <- [0 .. (tw-1)]] $ \(x, y) -> do
      let
         od = w * (y - oy + dy) + (x - ox + dx)
         os = tw * y + x
         
      unless ((y - oy + dy) >= w || (x - ox + dx) >= h) $
         MV.unsafeRead px od >>= \old -> MV.unsafeWrite px od $ pxAdd old (px' `V.unsafeIndex` os)
      
pxAdd :: Pixel -> Pixel -> Pixel
pxAdd 
   (w1, (r1, g1, b1), (r1', g1', b1'))
   (w2, (r2, g2, b2), (r2', g2', b2')) =
      (w1 + w2, (r1 + r2, g1 + g2, b1 + b2), (r1' + r2', g1' + g2', b1' + b2'))
      
addPixel :: PrimMonad m => MImage m -> (Int, Int, WeightedSpectrum) -> m ()
{-# INLINE addPixel #-}
addPixel (MImage w h (ox, oy) _ p) (x, y, (sw, s))
   | (x - ox) < 0 || (y - oy) < 0 = return ()
   | x >= (w + ox) || y >= (h + oy) = return ()
   | otherwise = MV.unsafeRead p o >>= \px -> MV.unsafeWrite p o (add px dpx)
   where
         dpx = (sw, r, g, b)
         (r, g, b) = toRGB s
         add (w1, (r1, g1, b1), splat) (w2, r2, g2, b2) =
              (w1+w2, (r1+r2, g1+g2, b1+b2), splat)
         o = (x - ox) + (y - oy) * w
         
splatSample :: PrimMonad m => MImage m -> ImageSample -> m ()
splatSample (MImage w h (iox, ioy) _ p) (ImageSample sx sy (sw, ss))
   | floor sx >= w || floor sy >= h || sx < 0 || sy < 0 = return ()
   | sNaN ss = trace ("not splatting NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("not splatting infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | isNaN sw = trace ("not splatting NaN weight sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | isInfinite sw = trace ("not splatting infinite weight sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "splatSample" #-} do
      (ow, oxyz, (ox, oy, oz)) <- MV.unsafeRead p o
      MV.unsafeWrite p o (ow, oxyz, (ox + dx, oy + dy, oz + dz))
      where
         o = ((floor sx - iox) + (floor sy - ioy) * w)
         (dx, dy, dz) = (\(x, y, z) -> (x * sw, y * sw, z * sw)) $ toRGB ss

-- | adds a sample to the specified image
addSample :: PrimMonad m => MImage m -> ImageSample -> m ()
{-# INLINE addSample #-}
addSample img smp@(ImageSample sx sy (sw, ss))
   | sw == 0 = return ()
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "addSample" #-} filterSample (imageFilter img) smp (addPixel img)

addContrib :: PrimMonad m => MImage m -> Contribution -> m ()
addContrib img (splat, is)
   | splat = splatSample img is
   | otherwise = addSample img is

-- | extracts the pixel at the specified offset from an Image
getPixel
   :: Image
   -> Float -- ^ splat weight
   -> Int
   -> (Float, Float, Float)
getPixel (Img _ _ _ p) sw o
   | w == 0 = (sw * sr, sw * sg, sw * sb)
   | otherwise = (sw * sr + r / w, sw * sg + g / w, sw * sb + b / w)
   where
      (w, (r, g, b), (sr, sg, sb)) = p V.! o
      
-- | writes an image in ppm format
writePpm
   :: Image
   -> Float -- ^ splat weight
   -> Handle
   -> IO ()
writePpm img@(Img w h _ _) splatW handle =
   let
       header = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
       pixel p = return $ ppmPixel $ getPixel img splatW p
   in do
      hPutStr handle header
      mapM_ (\p -> pixel p >>= hPutStr handle) [0..(w*h-1)]
      
-- | applies gamma correction to an RGB triple
gamma :: Float -> (Float, Float, Float) -> (Float, Float, Float)
gamma x (r, g, b) = let x' = 1 / x in (r ** x', g ** x', b ** x')

-- | converts a Float in [0..1] to an Int in [0..255], clamping values outside [0..1]
clamp :: Float -> Int
clamp v = round ( min 1 (max 0 v) * 255 )

rgbPixels :: Image -> Float -> SampleWindow -> [((Int, Int), (Int, Int, Int))]
rgbPixels img@(Img w h _ _) spw wnd = Prelude.zip xs clamped where
   ps = map (getPixel img spw) os
   rgbs = map (gamma 2.2) ps
   clamped = map (\(r,g,b) -> (clamp r, clamp g, clamp b)) rgbs
   xs = filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) $ coverWindow wnd
   os = map (\(x,y) -> (y * (imgW img)) + x) xs

-- | converts a @WeightedSpectrum@ into what's expected to be found in a ppm file
ppmPixel :: (Float, Float, Float) -> String
ppmPixel ws = (toString . gamma 2.2) ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "

--------------------------------------------------------------------------------
-- RGBE
--------------------------------------------------------------------------------

frexp :: Float -> (Float, Int)
frexp x
   | isNaN x = error "NaN given to frexp"
   | isInfinite x = error "infinity given to frexp"
   | otherwise  = frexp' (x, 0) where
      frexp' (s, e)
         | s >= 1.0 = frexp' (s / 2, e + 1)
         | s < 0.5 = frexp' (s * 2, e - 1)
         | otherwise = (s, e)

writeRgbe :: Image -> Float -> Handle -> IO ()
writeRgbe img@(Img w h _ _) spw hnd =
   let header = "#?RGBE\nFORMAT=32-bit_rgbe\n\n-Y " ++ show h ++ " +X " ++ show w ++ "\n"
       pixel :: Int -> IO BS.ByteString
       pixel p = return $ toRgbe $ getPixel img spw p
   in do
      hPutStr hnd header
      mapM_ (\p -> pixel p >>= BS.hPutStr hnd) [0..(w*h-1)]

toRgbe :: (Float, Float, Float) -> BS.ByteString
toRgbe (r, g, b)
   | v < 0.00001 = BS.pack [0,0,0,0]
   | otherwise = BS.pack $ map truncate [r * v'', g * v'', b * v'', fromIntegral $ e + 128]
   where
         v = max r $ max g b
         (v', e) = frexp v
         v'' = v' * 256 / v
