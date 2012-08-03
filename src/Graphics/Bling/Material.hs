
module Graphics.Bling.Material (
   
   mkMatte, glassMaterial, mirrorMaterial, mkPlastic, mkMetal, mkShinyMetal,
   mkSubstrate
   
   ) where
   
import Graphics.Bling.Reflection
import Graphics.Bling.Texture

-- | Creates a matte @Material@, which uses either a @Lambertian@ or
--   an @OrenNayar@ BxDF, depending on the roughness
mkMatte
   :: SpectrumTexture -- ^ the overall color
   -> ScalarTexture -- ^ the sigma (roughness) parameter
   -> Material
mkMatte tex ts dgg dgs
   | s == 0 = mkBsdf' [mkLambertian r] dgg dgs
   | otherwise = mkBsdf' [mkOrenNayar r s] dgg dgs
   where
      s = ts dgs
      r = tex dgs

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
mkShinyMetal kr ks rough dgg dgs = mkBsdf' [r, s] dgg dgs where
   r = mkMicrofacet (mkBlinn (1 / rough dgs)) frMf white
   s = mkSpecularReflection white frSr
   frMf = frConductor (approxEta $ ks dgs) black
   frSr = frConductor (approxEta $ kr dgs) black

approxEta :: Spectrum -> Spectrum
approxEta r = (white + r') / (white - r') where
   r' = sSqrt $ sClamp 0 0.999 r
   
mkSubstrate
   :: SpectrumTexture
   -> SpectrumTexture
   -> ScalarTexture
   -> ScalarTexture
   -> Material
mkSubstrate kd ks tur tvr dgg dgs = mkBsdf' [brdf] dgg dgs where
   brdf = mkFresnelBlend rd rs dist
   dist = mkAnisotropic (1 / u) (1 / v)
   u = tur dgs
   v = tvr dgs
   rd = sClamp 0 1 $ kd dgs
   rs = sClamp 0 1 $ ks dgs
   
