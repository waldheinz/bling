
{-# LANGUAGE UnboxedTuples #-}

module Graphics.Bling.Material.Specular (
   
   -- * Specular materials
   
   glassMaterial, mirrorMaterial,
   
   ) where

import Graphics.Bling.Reflection
import Graphics.Bling.Texture

-- | a glass material
glassMaterial
   :: ScalarTexture -- ^ index of refraction
   -> SpectrumTexture -- ^ reflection color
   -> SpectrumTexture -- ^ transmission color
   -> Material
glassMaterial iort rt tt dgg dgs = mkBsdf' [refl, trans] dgg dgs where
   refl = mkSpecularReflection r $ frDielectric 1 ior
   trans = mkSpecularTransmission t 1 ior
   r = sClamp' $ rt dgs
   t = sClamp' $ tt dgs
   ior = iort dgs
   
mirrorMaterial
   :: SpectrumTexture -- ^ reflection color
   -> Material
mirrorMaterial rt dgg dgs = mkBsdf' [bxdf] dgg dgs where
   bxdf = mkSpecularReflection r frNoOp
   r = sClamp 0 1 $ rt dgs

