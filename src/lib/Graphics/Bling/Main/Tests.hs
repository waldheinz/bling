
import Graphics.Bling.Random

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.List (sort)
import qualified System.Random.MWC as MWC
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: String -> Property
prop_reversereverse s = property $ (reverse . reverse) s == id s

allProps = [
   prop_reversereverse]

--------------------------------------------------------------------------------
-- Graphics.Bling.Random
--------------------------------------------------------------------------------

prop_rndIn01 :: [Int] -> Property
prop_rndIn01 seed = monadicST test where
   test = do
      x <- run $ runWithSeed (intSeed seed) rnd
      assert $ x >= 0
      assert $ x < 1

prop_shuffle_retains :: (String, [Int]) -> Property
prop_shuffle_retains (s, i) = monadicST test where
   test = do
      v' <- run $ runWithSeed (intSeed i) $ do
         v <- liftR $ V.thaw (V.fromList s)
         shuffle v
         liftR $ V.freeze v

      let s' = V.toList v'
      
      assert $ (sort s) == (sort s')
      
main :: IO ()
main = do
   putStrLn "Running tests..."
   mapM_ quickCheck allProps
   quickCheckWith (stdArgs { maxSuccess = 1000 }) prop_rndIn01
   quickCheckWith (stdArgs { maxSuccess = 1000 }) prop_shuffle_retains
   