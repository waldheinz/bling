
module Transform (
      Transform, identity, translate, scale, inverse, fromMatrix, rotateX,
      rotateY, rotateZ,
      transPoint, transVector, transBox, transRay, transNormal, concatTrans
   ) where

import AABB
import Math
import Data.List (transpose, foldl')

data Matrix = MkMatrix {
   m00 :: {-# UNPACK #-} !Flt, m01 :: {-# UNPACK #-} !Flt, m02 :: {-# UNPACK #-} !Flt, m03 :: {-# UNPACK #-} !Flt,
   m10 :: {-# UNPACK #-} !Flt, m11 :: {-# UNPACK #-} !Flt, m12 :: {-# UNPACK #-} !Flt, m13 :: {-# UNPACK #-} !Flt,
   m20 :: {-# UNPACK #-} !Flt, m21 :: {-# UNPACK #-} !Flt, m22 :: {-# UNPACK #-} !Flt, m23 :: {-# UNPACK #-} !Flt,
   m30 :: {-# UNPACK #-} !Flt, m31 :: {-# UNPACK #-} !Flt, m32 :: {-# UNPACK #-} !Flt, m33 :: {-# UNPACK #-} !Flt
   } deriving (Eq)

toList :: Matrix -> [[Flt]]
toList m = [
   [m00 m, m01 m, m02 m, m03 m],
   [m10 m, m11 m, m12 m, m13 m],
   [m20 m, m21 m, m22 m, m23 m],
   [m30 m, m31 m, m32 m, m33 m]]

fromList :: [[Flt]] -> Matrix
fromList (
   (l00:l01:l02:l03:[]):
   (l10:l11:l12:l13:[]):
   (l20:l21:l22:l23:[]):
   (l30:l31:l32:l33:[]):[]) = MkMatrix
   l00 l01 l02 l03
   l10 l11 l12 l13
   l20 l21 l22 l23
   l30 l31 l32 l33
fromList _ = error "malformed matrix"

-- | @Matrix@ multiply
mul :: Matrix -> Matrix -> Matrix
mul m1 m2 = fromList l where
   l = [[sum $ zipWith (*) row col | col <- transpose a] | row <- b]
   a = toList m1
   b = toList m2

-- | transposes a @Matrix@
transMatrix :: Matrix -> Matrix
transMatrix = fromList.transpose.toList

instance Show Matrix where
   show = show . toList

-- | An affine transformation
data Transform = MkTransform {
   _matrix :: Matrix,
   _inverted :: Matrix
   } deriving (Eq)

instance Show Transform where
   show (MkTransform m _) = show m

-- | Creates a @Transform@ from the two matrices
fromMatrix :: ([[Flt]], [[Flt]]) -> Transform
fromMatrix (m, i) = MkTransform (fromList m) (fromList i)

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

rotateX :: Flt -> Transform
rotateX deg = MkTransform m (transMatrix m) where
   m = MkMatrix
      1    0       0 0
      0 cost (-sint) 0
      0 sint    cost 0
      0    0       0 1
   sint = sin (radians deg)
   cost = cos (radians deg)
      
rotateY :: Flt -> Transform
rotateY deg = MkTransform m (transMatrix m) where
   m = MkMatrix
        cost  0 sint 0
         0    1    0 0
      (-sint) 0 cost 0
         0    0    0 1
   cost = cos (radians deg)
   sint = sin (radians deg)
      
rotateZ :: Flt -> Transform
rotateZ deg = MkTransform m (transMatrix m) where
   m = MkMatrix
      cost (-sint) 0 0
      sint   cost  0 0
      0      0     1 0
      0      0     0 1
   sint = sin (radians deg)
   cost = cos (radians deg)

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
{-# INLINE transVector #-}
transVector (MkTransform m _) (MkVector x y z) = MkVector xp yp zp where
   xp = m00 m * x + m01 m * y + m02 m * z
   yp = m10 m * x + m11 m * y + m12 m * z
   zp = m20 m * x + m21 m * y + m22 m * z

-- | Applies a @Transform@ to a @Normal@
transNormal :: Transform -> Normal -> Normal
{-# INLINE transNormal #-}
transNormal (MkTransform m _) (MkVector x y z) = mkNormal xp yp zp where
   xp = m00 m * x + m10 m * y + m20 m * z
   yp = m01 m * x + m11 m * y + m21 m * z
   zp = m02 m * x + m12 m * y + m22 m * z

-- | Applies a @Transform@ to a @Ray@
transRay :: Transform -> Ray -> Ray
{-# INLINE transRay #-}
transRay t (Ray ro rd tmin tmax) =
   Ray (transPoint t ro) (transVector t rd) tmin tmax

-- | Applies a @Transform@ to an @AABB@
transBox :: Transform -> AABB -> AABB
{-# INLINE transBox #-}
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
   
