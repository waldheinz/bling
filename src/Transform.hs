
module Transform (
      Transform, identity, translate, inverse,
      transPoint, transVector
   ) where
      
import Math
      

data Matrix = MkMatrix {
   m00 :: Flt, m01 :: Flt, m02 :: Flt, m03 :: Flt,
   m10 :: Flt, m11 :: Flt, m12 :: Flt, m13 :: Flt,
   m20 :: Flt, m21 :: Flt, m22 :: Flt, m23 :: Flt,
   m30 :: Flt, m31 :: Flt, m32 :: Flt, m33 :: Flt
   }
   
-- | An affine transformation
data Transform = MkTransform {
   matrix :: Matrix,
   inverted :: Matrix
   }

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
