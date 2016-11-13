import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts
       [ testCase "test1" test1
       , testProperty "test2" test2
       ] mempty


test1 :: Assertion
test1 = sum [1, 2, 3, 4] @?= 10

test2 :: [Integer] -> Bool
test2 xs = xs == (reverse . reverse) xs
