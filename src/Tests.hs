
import Char
import List
import Test.QuickCheck
import Text.Printf

import Montecarlo

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: String -> Property
prop_reversereverse s = property $ (reverse . reverse) s == id s



tests = [
   prop_reversereverse]
