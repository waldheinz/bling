
module AABB (
   AABB, aabbMin, aabbMax,
   emptyAABB, extendAABB, extendAABBP, maximumExtent, centroid, intersectAABB
) where

import Foreign.Storable
import Foreign

import Math

-- | an axis-aligned bounding box
data AABB = AABB {
   aabbMin :: Point, -- ^ the box' minimum
   aabbMax :: Point  -- ^ the box' maximum
   } deriving Show

instance Storable AABB where
   sizeOf _ = sizeOf (undefined :: Flt) * 6
   alignment _ = alignment (undefined :: Flt)
   
   peek p = do
      x1 <- peekElemOff q 0 :: IO Flt
      y1 <- peekElemOff q 1 :: IO Flt
      z1 <- peekElemOff q 2 :: IO Flt
      x2 <- peekElemOff q 3 :: IO Flt
      y2 <- peekElemOff q 4 :: IO Flt
      z2 <- peekElemOff q 5 :: IO Flt
      return (AABB (x1, y1, z1) (x2, y2, z2)) where
	 q = castPtr p
      
   poke p (AABB (x1, y1, z1) (x2, y2, z2)) = do
      pokeElemOff q 0 x1
      pokeElemOff q 0 y1
      pokeElemOff q 0 z1
      pokeElemOff q 0 x2
      pokeElemOff q 0 y2
      pokeElemOff q 0 z2
      where
	 q = castPtr p

emptyAABB :: AABB
emptyAABB = AABB (infinity, infinity, infinity) (-infinity, -infinity, -infinity)

extendAABB :: AABB -> AABB -> AABB
extendAABB (AABB (min1x, min1y, min1z) (max1x, max1y, max1z)) (AABB (min2x, min2y, min2z) (max2x, max2y, max2z)) =
   AABB (min min1x min2x, min min1y min2y, min min1z min2z) (max max1x max2x, max max1y max2y, max max1z max2z)

extendAABBP :: AABB -> Point -> AABB
extendAABBP (AABB pMin pMax) p = AABB (f min pMin p) (f max pMax p) where
   f cmp (x1, y1, z1) (x2, y2, z2) = (cmp x1 x2, cmp y1 y2, cmp z1 z2)

-- | finds the @Dimension@ along which an @AABB@ has it's maximum extent
maximumExtent :: AABB -> Dimension
maximumExtent (AABB pmin pmax) = dominant $ sub pmax pmin

centroid :: AABB -> Point
centroid (AABB pmin pmax) = add pmin $ scalMul (sub pmax pmin) 0.5 

intersectAABB :: AABB -> Ray -> Maybe (Flt, Flt)
intersectAABB (AABB bMin bMax) (Ray o d tmin tmax) = testSlabs allDimensions tmin tmax where
   testSlabs :: [Dimension] -> Flt -> Flt -> Maybe (Flt, Flt)
   testSlabs [] n f
      | n > f = Nothing
      | otherwise = Just (n, f)
   testSlabs (dim:ds) near far
      | near > far = Nothing
      | otherwise = testSlabs ds (max near near') (min far far') where
	 (near', far') = if tNear > tFar then (tFar, tNear) else (tNear, tFar)
	 tFar = ((component bMax dim) - oc) * dInv
	 tNear = ((component bMin dim) - oc) * dInv
	 oc = component o dim
	 dInv = 1 / (component d dim)
   