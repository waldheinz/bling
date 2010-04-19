
import Bsdf
import Geometry
import Material
import Math
import Random

import System.Random

material :: Matte
material = Matte (0.8, 0.8, 0.8)

int :: Intersection
int = Intersection 10 (0, 0, 0) (0, 1, 0) 

bsdf :: Bsdf
bsdf = materialBsdf material int

wo :: Vector
wo = normalize (-1, -1, 0)

sphericalToVector :: (Float, Float) -> Vector
sphericalToVector (omega, phi) = (x, y, z) where
   x = sin omega * cos phi
   y = sin omega * sin phi
   z = cos omega

directions :: Int -> [Vector]
directions 0 = []
directions n = map sphericalToVector sp where
   sp = [(o, p) | o <- [0, step .. pi], p <- [0, step .. 2*pi]]
   step = 1.0 / (fromIntegral n)

vecs :: Int -> [Vector]

vecs 0 = []
--vecs n = do
--   sample <- sampleBsdf bsdf wo
--   v <- randomOnSphere
--   v <- cosineSampleHemisphere
--   rest <- vecs (n-1)
--   return ((bsdfSampleWi sample) : rest)
   
vecPlot :: [Vector] -> String
vecPlot [] = ""
vecPlot ((x, y, z) : xs) = show x ++ "\t" ++ show y ++ "\t" ++ show z ++ "\n" ++ vecPlot xs

f = id -- (evalBsdf bsdf wo)

main :: IO ()
main = do
   putStrLn $ vecPlot $ (map f (directions 20))
   -- prng <- newStdGen
   -- putStrLn $ vecPlot $ fromRand $ runRand prng (vecs 100)
   