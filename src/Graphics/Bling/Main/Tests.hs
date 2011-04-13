
import Test.QuickCheck

import Graphics.Bling.Shape

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: String -> Property
prop_reversereverse s = property $ (reverse . reverse) s == id s

allProps = [
   prop_reversereverse]
   
main :: IO ()
main = do
   putStrLn "Running tests..."
   mapM_ quickCheck allProps
