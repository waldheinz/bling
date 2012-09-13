
module Graphics.Bling.Material (
   
   MaterialMap, mkMaterialMap,
   
   mkMatte, glassMaterial, mirrorMaterial, mkPlastic, mkMetal, mkShinyMetal,
   mkSubstrate, translucentMatte
   
   ) where
   
import qualified Data.Map as Map
import Debug.Trace

import Graphics.Bling.Fresnel
import Graphics.Bling.Reflection
import Graphics.Bling.Reflection.Diffuse
import Graphics.Bling.Reflection.Microfacet
import Graphics.Bling.Reflection.Specular
import Graphics.Bling.Texture

type MaterialMap = String -> Material

mkMaterialMap
   :: Material                -- ^ default material (on name not found)
   -> [(String, Material)]    -- ^ (name, material) mappings
   -> MaterialMap
mkMaterialMap def ms name = Map.findWithDefault (traceShow name $ def) name m where
   m = Map.unions $ map (\(n, mat) -> Map.singleton n mat) ms

-- | Creates a matte @Material@, which uses either a @Lambertian@ or
--   an @OrenNayar@ BxDF, depending on the roughness
mkMatte
   :: SpectrumTexture   -- ^ the overall color
   -> ScalarTexture     -- ^ the sigma (roughness) parameter
   -> Material
mkMatte tex ts dgg dgs
   | s == 0 = mkBsdf' [mkLambertian r] dgg dgs
   | otherwise = mkBsdf' [mkOrenNayar r s] dgg dgs
   where
      s = ts dgs
      r = tex dgs

translucentMatte
   :: SpectrumTexture   -- ^ reflected
   -> SpectrumTexture   -- ^ transmitted
   -> ScalarTexture     -- ^ roughness
   -> Material
translucentMatte kr kt ks dgg dgs = mkBsdf' [refl, trans] dgg dgs where
   refl = if s == 0 then mkLambertian r else mkOrenNayar r s
   trans = brdfToBtdf $ if s == 0 then mkLambertian t else mkOrenNayar t s
   r = sClamp 0 1 $ kr dgs
   t = (sClamp 0 1 $ kt dgs) * (white - r)
   s = ks dgs
   
-- | a glass material
glassMaterial
   :: ScalarTexture -- ^ index of refraction
   -> SpectrumTexture -- ^ reflection color
   -> SpectrumTexture -- ^ transmission color
   -> Material
glassMaterial iort rt tt dgg dgs = mkBsdf' [refl, trans] dgg dgs where
   refl = specRefl (frDielectric 1 ior) r
   trans = specTrans t 1 ior
   r = sClamp' $ rt dgs
   t = sClamp' $ tt dgs
   ior = iort dgs
   
mirrorMaterial
   :: SpectrumTexture -- ^ reflection color
   -> Material
mirrorMaterial rt dgg dgs = mkBsdf' [bxdf] dgg dgs where
   bxdf = specRefl frNoOp $ sClamp 0 1 $ rt dgs
   
-- | Creates a plasic @Material@ using lambertian reflection for the base
--   color and a Blinn microfacet distribution for the specular component
mkPlastic
   :: SpectrumTexture -- ^ diffuse spectrum
   -> SpectrumTexture -- ^ specular spectrum
   -> ScalarTexture -- ^ roughness
   -> Material
mkPlastic kd ks kr dgg dgs = mkBsdf' [diff, spec] dgg dgs where
   diff = mkLambertian rd
   spec = mkMicrofacet (mkBlinn (1 / rough)) (frDielectric 1.0 1.5) rs
   rough = kr dgs
   rd = kd dgs
   rs = ks dgs

-- | Creates a Metal @Material@
mkMetal
   :: SpectrumTexture -- ^ eta
   -> SpectrumTexture -- ^ k
   -> ScalarTexture -- ^ roughness
   -> Material
mkMetal eta k rough dgg dgs = mkBsdf' [spec] dgg dgs where
   fr = frConductor (eta dgs) (k dgs)
   spec = mkMicrofacet (mkBlinn (1 / rough dgs)) fr white

mkShinyMetal
   :: SpectrumTexture -- ^ kr
   -> SpectrumTexture -- ^ ks
   -> ScalarTexture -- ^ roughness
   -> Material
mkShinyMetal kr ks rough dgg dgs = mkBsdf' [diff, spec] dgg dgs where
   diff = mkMicrofacet (mkBlinn (1 / rough dgs)) frMf white
   spec = specRefl frSr white
   frMf = frConductor (frApproxEta s) (frApproxK s)
   frSr = frConductor (frApproxEta r) (frApproxK r)
   r = kr dgs
   s = ks dgs
   
mkSubstrate
   :: SpectrumTexture   -- ^ diffuse
   -> SpectrumTexture   -- ^ specular
   -> SpectrumTexture   -- ^ absorption
   -> ScalarTexture     -- ^ anisotropic u
   -> ScalarTexture     -- ^ anisotropic v
   -> ScalarTexture     -- ^ coating thickness
   -> Material
mkSubstrate kd ks ka tur tvr td dgg dgs = mkBsdf' [brdf] dgg dgs where
   brdf = mkFresnelBlend rd rs ra dist depth
   dist = mkAnisotropic (1 / u) (1 / v)
   u = max 0 $ tur dgs
   v = max 0 $ tvr dgs
   rd = sClamp 0 1 $ kd dgs
   rs = sClamp 0 1 $ ks dgs
   ra = sClamp 0 1 $ ka dgs
   depth = td dgs
   
