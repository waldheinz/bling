
module Graphics.Bling.Transform (
      module Graphics.Bling.Math,
      module Graphics.Bling.AABB,

      Transform, translate, scale, inverse, fromMatrix, rotateX,
      rotateY, rotateZ, lookAt, perspective,
      transPoint, transVector, transBox, transRay, transNormal,
      fromMatrix',

      -- * Utility Functions

      solveLinearSystem2x2

   ) where

import Graphics.Bling.AABB
import Graphics.Bling.Math

import Control.Monad (forM, forM_, unless)
import Control.Monad.ST
import Data.Function (on)
import Data.Monoid
import Data.List (foldl', maximumBy)
import Data.STRef
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

newtype Matrix = Matrix { unM :: UV.Vector Float }

matrix ::
   Float -> Float -> Float -> Float ->
   Float -> Float -> Float -> Float ->
   Float -> Float -> Float -> Float ->
   Float -> Float -> Float -> Float -> Matrix
matrix a b c d e f g h i j k l m n o p = Matrix $ UV.fromList
   [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]

-- | index into a matrix
mi :: Matrix -> Int -> Int -> Float
{-# INLINE mi #-}
mi m r c = UV.unsafeIndex (unM m) ((r * 4) + c)

invert :: Matrix -> Matrix
invert m = runST $ do
   minv <- UV.thaw $ unM m
   ipiv <- newSTRef [0..3]
   indx <- UV.generateM 4 $ const $ do
      (irow, icol) <- readSTRef ipiv >>= pivot minv
      modifySTRef ipiv (filter (/= icol))

      -- swap rows irow and icol for pivot
      unless (irow == icol) $ forM_ [0..3] $ \k ->
         MUV.swap minv (idx irow k) (idx icol k)

      -- set m[icol][icol] to one by scaling row icol
      pivinv <- (/) 1 <$> MUV.read minv (idx icol icol)
      MUV.write minv (idx icol icol) 1
      forM_ [0..3] $ \j -> let i = idx icol j
         in MUV.read minv i >>= \v -> MUV.write minv i (v * pivinv)

      -- subtract this row from others to zero out their columns
      forM_ [0..3] $ \j -> do
         unless (j == icol) $ do
            save <- MUV.read minv $ idx j icol
            MUV.write minv (idx j icol) 0

            forM_ [0..3] $ \k -> do
               x <- MUV.read minv $ idx icol k
               old <- MUV.read minv (idx j k)
               MUV.write minv (idx j k ) $ old - x * save

      return (irow, icol)

   -- swap columns to reflect permutation
   UV.forM_ (UV.reverse indx) $ \(ir, ic) -> unless (ir == ic) $ do
      forM_ [0..3] $ \k -> MUV.swap minv (idx k ir) (idx k ic)

   Matrix <$> UV.freeze minv
   where
      idx :: Int -> Int -> Int
      idx r c = c * 4 + r

      pivot :: MUV.MVector s Float -> [Int] -> ST s (Int, Int)
      pivot minv is = fst . maximumBy (compare `on` snd) <$>
         forM [(j,k) | j <- is, k <- is] pvt where
            pvt jk@(j, k) = MUV.read minv (idx j k) >>= \v -> return (jk, abs v)

fromLists :: [[Float]] -> Matrix
fromLists (
   (l00:l01:l02:l03:[]):
   (l10:l11:l12:l13:[]):
   (l20:l21:l22:l23:[]):
   (l30:l31:l32:l33:[]):[]) = matrix
   l00 l01 l02 l03
   l10 l11 l12 l13
   l20 l21 l22 l23
   l30 l31 l32 l33
fromLists _ = error "malformed matrix"

-- | @Matrix@ multiply
mul :: Matrix -> Matrix -> Matrix
mul m1 m2 = Matrix $ UV.generate 16 go where
   go n = sum $ [mi m1 k j *  mi m2 i k | k <- [0..3]] where
      (i, j) = divMod n 4

-- | transposes a @Matrix@
transMatrix :: Matrix -> Matrix
transMatrix m = Matrix $ UV.generate 16 go where
   go n = let (i, j) = divMod n 4 in mi m j i

idMatrix :: Matrix
idMatrix = matrix
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1

instance Show Matrix where
   show mt = let (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:[]) = UV.toList (unM mt) in
      show [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

-- | An affine transformation
data Transform = MkTransform {
   _matrix     :: ! Matrix,
   _inverted   :: ! Matrix
   }

instance Show Transform where
   show (MkTransform m i) = show m ++ "\n" ++ show i

instance Monoid Transform where
   mempty = identity
   mappend = concatTrans

-- | Creates a @Transform@ from the two matrices
fromMatrix :: ([[Float]], [[Float]]) -> Transform
fromMatrix (m, i) = MkTransform (fromLists m) (fromLists i)

fromMatrix' :: [[Float]] -> Transform
fromMatrix' m = MkTransform (fromLists m) (invert (fromLists m))

-- | The identity transformation
identity :: Transform
identity = MkTransform idMatrix idMatrix

-- | Translates by the specified distance
translate :: Vector -> Transform
translate (Vector dx dy dz) = MkTransform m i where
   m = matrix
      1 0 0 dx
      0 1 0 dy
      0 0 1 dz
      0 0 0 1

   i = matrix
      1 0 0 (-dx)
      0 1 0 (-dy)
      0 0 1 (-dz)
      0 0 0 1

-- | Scales by the specified amount
scale :: Vector -> Transform
scale (Vector sx sy sz) = MkTransform m i where
   m = matrix
      sx 0  0  0
      0 sy  0  0
      0  0 sz  0
      0  0  0  1
   i = matrix
      (1/sx) 0 0 0
      0 (1/sy) 0 0
      0 0 (1/sz) 0
      0 0 0 1

rotateX :: Float -> Transform
rotateX deg = MkTransform m (transMatrix m) where
   m = matrix
      1    0       0 0
      0 cost (-sint) 0
      0 sint    cost 0
      0    0       0 1
   sint = sin (radians deg)
   cost = cos (radians deg)

rotateY :: Float -> Transform
rotateY deg = MkTransform m (transMatrix m) where
   m = matrix
        cost  0 sint 0
         0    1    0 0
      (-sint) 0 cost 0
         0    0    0 1
   cost = cos (radians deg)
   sint = sin (radians deg)

rotateZ :: Float -> Transform
rotateZ deg = MkTransform m (transMatrix m) where
   m = matrix
      cost (-sint) 0 0
      sint   cost  0 0
      0      0     1 0
      0      0     0 1
   sint = sin (radians deg)
   cost = cos (radians deg)

-- | creates a perspective transform
perspective
   :: Float -- ^ the field of view in degrees
   -> Float -- ^ the near clipping plane
   -> Float -- ^ the far clippping plane
   -> Transform
perspective fov n f = scale s <> (MkTransform m (invert m)) where
   s = Vector iTanAng iTanAng 1
   iTanAng = 1 / tan (radians fov / 2)
   m = matrix
      1 0 0 0
      0 1 0 0
      0 0 (f / (f - n)) (-f*n / (f - n))
      0 0 1 0

-- | Creates a "look at" @Transform@
lookAt
   :: Point -- ^ the observer position
   -> Point -- ^ the point to look at
   -> Vector -- ^ the up vector
   -> Transform
lookAt p@(Vector px py pz) l up = MkTransform m (invert m) where
   m = fromLists [
      [lx, ux, dx, px],
      [ly, uy, dy, py],
      [lz, uz, dz, pz],
      [ 0,  0,  0,  1]]
   dir@(Vector dx dy dz) = normalize (l - p)
   left@(Vector lx ly lz) = normalize $ (normalize up) `cross` dir
   (Vector ux uy uz) = dir `cross` left

-- | Creates the inverse of a given @Transform@.
inverse :: Transform -> Transform
inverse (MkTransform m i) = MkTransform i m

concatTrans :: Transform -> Transform -> Transform
concatTrans (MkTransform m1 i1) (MkTransform m2 i2) = MkTransform m' i' where
   m' = mul m1 m2
   i' = mul i2 i1

-- | Applies a @Transform@ to a @Point@
transPoint :: Transform -> Point -> Point
{-# INLINE transPoint #-}
transPoint (MkTransform m _) (Vector x y z)
   | wp == 1 = mkPoint' xp yp zp
   | otherwise = mkPoint (xp/wp, yp/wp, zp/wp)
   where
      xp = mi m 0 0 * x + mi m 0 1 * y + mi m 0 2 * z + mi m 0 3
      yp = mi m 1 0 * x + mi m 1 1 * y + mi m 1 2 * z + mi m 1 3
      zp = mi m 2 0 * x + mi m 2 1 * y + mi m 2 2 * z + mi m 2 3
      wp = mi m 3 0 * x + mi m 3 1 * y + mi m 3 2 * z + mi m 3 3

-- | Applies a @Transform@ to a @Vector@
transVector :: Transform -> Vector -> Vector
{-# INLINE transVector #-}
transVector (MkTransform m _) (Vector x y z) = Vector xp yp zp where
   xp = mi m 0 0 * x + mi m 0 1 * y + mi m 0 2 * z
   yp = mi m 1 0 * x + mi m 1 1 * y + mi m 1 2 * z
   zp = mi m 2 0 * x + mi m 2 1 * y + mi m 2 2 * z

-- | Applies a @Transform@ to a @Normal@
transNormal :: Transform -> Normal -> Normal
{-# INLINE transNormal #-}
transNormal (MkTransform _ m) (Vector x y z) = mkNormal xp yp zp where
   xp = mi m 0 0 * x + mi m 1 0 * y + mi m 2 0 * z
   yp = mi m 0 1 * x + mi m 1 1 * y + mi m 2 1 * z
   zp = mi m 0 2 * x + mi m 1 2 * y + mi m 2 2 * z

-- | Applies a @Transform@ to a @Ray@
transRay :: Transform -> Ray -> Ray
{-# INLINE transRay #-}
transRay t (Ray ro rd tmin tmax) =
   Ray (transPoint t ro) (transVector t rd) tmin tmax

-- | Applies a @Transform@ to an @AABB@
transBox :: Transform -> AABB -> AABB
{-# INLINE transBox #-}
transBox t (AABB (Vector mx my mz) (Vector nx ny nz)) = b where
   b  = foldl' extendAABBP mempty [p0, p1, p2, p3, p4, p5, p6, p7]
   p0 = transPoint t (mkPoint' mx my mz)
   p1 = transPoint t (mkPoint' mx my nz)
   p2 = transPoint t (mkPoint' mx ny mz)
   p3 = transPoint t (mkPoint' mx ny nz)
   p4 = transPoint t (mkPoint' nx my mz)
   p5 = transPoint t (mkPoint' nx my nz)
   p6 = transPoint t (mkPoint' nx ny mz)
   p7 = transPoint t (mkPoint' nx ny nz)

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

solveLinearSystem2x2 :: (Float, Float, Float, Float) -> (Float, Float) -> Maybe (Float, Float)
solveLinearSystem2x2 (a00, a01, a10, a11) (b0, b1)
   | abs det < 1e-10 = Nothing
   | isNaN x0 || isNaN x1 = Nothing
   | otherwise = Just (x0, x1)
   where
      det = a00 * a11 - a01 * a10
      x0 = (a11 * b0 - a01 * b1) / det
      x1 = (a00 * b1 - a10 * b0) / det
