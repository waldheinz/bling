
module Transform (
      Transform, identity, translate, scale, inverse,
      transPoint, transVector, transBox, concatTrans
   ) where

import AABB
import Math
import Data.List (transpose, foldl')

data Matrix = MkMatrix {
   m00 :: Flt, m01 :: Flt, m02 :: Flt, m03 :: Flt,
   m10 :: Flt, m11 :: Flt, m12 :: Flt, m13 :: Flt,
   m20 :: Flt, m21 :: Flt, m22 :: Flt, m23 :: Flt,
   m30 :: Flt, m31 :: Flt, m32 :: Flt, m33 :: Flt
   }

toList :: Matrix -> [[Flt]]
toList m = [
   [m00 m, m01 m, m02 m, m03 m],
   [m10 m, m11 m, m12 m, m13 m],
   [m20 m, m21 m, m22 m, m23 m],
   [m30 m, m31 m, m32 m, m33 m]]

fromList :: [[Flt]] -> Matrix
fromList (
   (m00:m01:m02:m03:[]):
   (m10:m11:m12:m13:[]):
   (m20:m21:m22:m23:[]):
   (m30:m31:m32:m33:[]):[]) = MkMatrix
   m00 m01 m02 m03
   m10 m11 m12 m13
   m20 m21 m22 m23
   m30 m31 m32 m33

mul :: Matrix -> Matrix -> Matrix
mul m1 m2 = fromList l where
   l = [[sum $ zipWith (*) row col | col <- transpose a] | row <- b]
   a = toList m1
   b = toList m2
   
instance Show Matrix where
   show = show . toList

-- | An affine transformation
data Transform = MkTransform {
   matrix :: Matrix,
   inverted :: Matrix
   }

instance Show Transform where
   show (MkTransform m _) = show m

-- | The identity transformation
identity :: Transform
identity = MkTransform m m where
   m = MkMatrix
      1 0 0 0
      0 1 0 0
      0 0 1 0
      0 0 0 1

-- | Translates by the specified distance
translate :: Vector -> Transform
translate (MkVector dx dy dz) = MkTransform m i where
   m = MkMatrix
      1 0 0 dx
      0 1 0 dy
      0 0 1 dz
      0 0 0 1
      
   i = MkMatrix
      1 0 0 (-dx)
      0 1 0 (-dy)
      0 0 1 (-dz)
      0 0 0 1

-- | Scales by the specified amount
scale :: Vector -> Transform
scale (MkVector sx sy sz) = MkTransform m i where
   m = MkMatrix
      sx 0  0  0
      0 sy  0  0
      0  0 sz  0
      0  0  0  1
   i = MkMatrix
      (1/sx) 0 0 0
      0 (1/sy) 0 0
      0 0 (1/sz) 0
      0 0 0 1

-- | Creates the inverse of a given @Transform@.
inverse :: Transform -> Transform
inverse (MkTransform m i) = MkTransform i m

concatTrans :: Transform -> Transform -> Transform
concatTrans (MkTransform m1 i1) (MkTransform m2 i2) = MkTransform m' i' where
   m' = mul m1 m2
   i' = mul i2 i1

-- | Applies a @Transform@ to a @Point@
transPoint :: Transform -> Point -> Point
transPoint (MkTransform m _) (MkVector x y z)
   | wp == 1 = mkPoint xp yp zp
   | otherwise = mkPoint (xp/wp) (yp/wp) (zp/wp)
   where
      xp = m00 m * x + m01 m * y + m02 m * z + m03 m
      yp = m10 m * x + m11 m * y + m12 m * z + m13 m
      zp = m20 m * x + m21 m * y + m22 m * z + m23 m
      wp = m30 m * x + m31 m * y + m32 m * z + m33 m

-- | Applies a @Transform@ to a @Vector@
transVector :: Transform -> Vector -> Vector
transVector (MkTransform m _) (MkVector x y z) = MkVector xp yp zp where
   xp = m00 m * x + m01 m * y + m02 m * z
   yp = m10 m * x + m11 m * y + m12 m * z
   zp = m20 m * x + m21 m * y + m22 m * z

-- | Applies a @Transform@ to a @Normal@
transNormal :: Transform -> Normal -> Normal
transNormal (MkTransform m _) (MkVector x y z) = mkNormal xp yp zp where
   xp = m00 m * x + m10 m * y + m20 m * z
   yp = m01 m * x + m11 m * y + m21 m * z
   zp = m02 m * x + m12 m * y + m22 m * z

-- | Applies a @Transform@ to a @Ray@
transRay :: Transform -> Ray -> Ray
transRay t (Ray ro rd tmin tmax) =
   Ray (transPoint t ro) (transVector t rd) tmin tmax

-- | Applies a @Transform@ to an @AABB@
transBox :: Transform -> AABB -> AABB
transBox t (AABB (MkVector mx my mz) (MkVector nx ny nz)) = b' where
   b' = foldl' extendAABBP emptyAABB [p0, p1, p2, p3, p4, p5, p6, p7]
   p0 = transPoint t (mkPoint mx my mz)
   p1 = transPoint t (mkPoint mx my nz)
   p2 = transPoint t (mkPoint mx ny mz)
   p3 = transPoint t (mkPoint mx ny nz)
   p4 = transPoint t (mkPoint nx my mz)
   p5 = transPoint t (mkPoint nx my nz)
   p6 = transPoint t (mkPoint nx ny mz)
   p7 = transPoint t (mkPoint nx ny nz)
   
