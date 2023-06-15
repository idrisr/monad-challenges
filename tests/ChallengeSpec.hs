module Main where

import Test.Hspec
import Challenge (fiveRands)

fiveRandsTests :: Spec
fiveRandsTests = do
    let ans = (8681089573064486461641871805074254223660 :: Integer)
    it "gets the right answer" $ do
        product fiveRands `shouldBe` ans

main :: IO ()
main = hspec $ do
    fiveRandsTests
