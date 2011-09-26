
import Graphics.Bling.Random

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: String -> Property
prop_reversereverse s = property $ (reverse . reverse) s == id s

allProps = [
   prop_reversereverse]

-- prop_rndIn01 :: Int -> Property
prop_rndIn01 seed = monadicST test where
   test = do
      let s = intSeed seed
      x <- run $ runWithSeed (intSeed seed) rnd
      assert $ x >= 0
      assert $ x < 1

main :: IO ()
main = do
   putStrLn "Running tests..."
   mapM_ quickCheck allProps
   quickCheckWith (stdArgs { maxSuccess = 100000 }) prop_rndIn01
