
module Geometry where

import AABB
import Math
import Random

import Data.Maybe

data DifferentialGeometry = DifferentialGeometry {
   dgP :: Point,
   dgN :: Normal
   } deriving (Show)

shadingCs :: DifferentialGeometry -> LocalCoordinates
shadingCs dg = coordinateSystem $ dgN dg

class Geometry a where
   -- | intersects a ray with an object, possibly returning the distance
   --   along the ray where an intersection occured together with the
   --   object properties at the intersection point
   intersect :: a -> Ray -> Maybe (Float, DifferentialGeometry)
   
   -- | decides if a ray intersects the object
   intersects :: a -> Ray -> Bool
   
   -- | the default implementation just test if @intersect@ returns something,
   --   should be overridded if this test can be performed cheaper
   intersects a r = isJust $ intersect a r
   
   -- | returns the surface area of the object
   boundArea :: a -> Float
   
   -- | returns a random point (along with its normal) on the object, 
   --   which is preferably visible from the specified point
   boundSample :: a -> Point -> Rand2D -> (Point, Normal)

   -- | returns the pdf for the sample chosen by @boundSample@
   boundPdf :: a -> Point -> Float
   
   -- | the default implementation returns the inverse of the area,
   --   which is suitable for a @boundSample@ implementation which
   --   chooses points uniformly on the surface
   boundPdf a _ = 1.0 / boundArea a
   
   localBounds :: a -> AABB
   
   worldBounds :: a -> AABB
   
-- | a sphere has a radius and a position
data Sphere = Sphere Float Point deriving Eq

insideSphere :: Sphere -> Point -> Bool
insideSphere (Sphere r pos) pt = sqLen (sub pos pt) - r * r < epsilon

sampleSphere :: Point -> Point -> Point
sampleSphere co pt
   | isNothing roots = pt
   | otherwise = normalize chosen
      where
         chosen = add co $ scalMul rd t
         rd = pt `sub` co
         (t, _) = fromJust roots
         roots = solveQuadric a b c
         a = sqLen rd
         b = 2 * (co `dot` rd)
         c = sqLen co - 1
         
instance Geometry Sphere where
   boundArea (Sphere r _) = r * r * 4 * pi
   
   localBounds = worldBounds
   
   worldBounds (Sphere r p) = emptyAABB `extendAABBP`
      (p `add` (MkVector r r r)) `extendAABBP`
      (p `add` (MkVector (-r) (-r) (-r)))
   
   boundSample sp@(Sphere r center) p us
      | insideSphere sp p = 
         let rndPt = randomOnSphere us 
             in (add center $ scalMul rndPt r, rndPt) -- sample full sphere if inside
      
      | otherwise = (ps, normalize $ sub ps center) where -- sample only the visible part if outside
         d = uniformSampleCone cs cosThetaMax us
         cs = coordinateSystem dn
         dn = normalize $ sub center p
         cosThetaMax = sqrt $ max 0 (1 - (r * r) / sqLen (sub p center))
         ps
            | isJust int = positionAt ray t
            | otherwise = sub center $ scalMul dn r
            where
                  ray = Ray p d 0 infinity
                  int = intersect sp ray
                  t = fst $ fromJust int
         
   boundPdf sp@(Sphere r center) pos
      | insideSphere sp pos = 1.0 / boundArea sp
      | otherwise = uniformConePdf cosThetaMax
      where
         cosThetaMax = sqrt $ max 0 (1 - r * r / sqLen (sub pos center))
   
   intersect (Sphere r center) ray@(Ray origin rd tmin tmax)
      | isNothing times = Nothing
      | t1 > tmax = Nothing
      | t2 < tmin = Nothing
      | otherwise = Just (t, DifferentialGeometry hitPoint normalAt)
      where
         co = origin `sub` center
         a = sqLen rd
         b = 2 * (co `dot` rd)
         c = sqLen co - (r * r)
         t = if t1 > tmin then t1 else t2
         (t1, t2) = fromJust times
         times = solveQuadric a b c
         hitPoint = positionAt ray t
         normalAt = normalize $ sub hitPoint center

   intersects (Sphere rad sc) (Ray ro rd tmin tmax)
      | isNothing roots = False
      | otherwise = t0 < tmax && t1 > tmin
      where
            dst = ro `sub` sc
            a = sqLen rd
            b = 2 * dot dst rd
            rad' = rad - epsilon
            c = sqLen dst - (rad' * rad')
            (t0, t1) = fromJust roots
            roots = solveQuadric a b c
            
