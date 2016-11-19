import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import ImageUtils
import ElTiempoEs

main :: IO ()
main = defaultMainWithOpts
       [ testCase "testLocateInRadar" testLocateInRadar
       , testProperty "test2" test2
       ] mempty


test2 :: [Integer] -> Bool
test2 xs = xs == (reverse . reverse) xs

testLocateInRadar :: Assertion
testLocateInRadar = locateIn radar (GpsCoord 37.171700 (-7.381614)) @?= ImgCoord 118 420
