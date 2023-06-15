module Main where

import Challenge (fiveRands, generalB, randEven, randLetter, randOdd, randPair, randString3, randTen)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Digest.Pure.SHA (sha256, showDigest)
import MCPrelude (mkSeed, rand)
import Test.Hspec

fiveRandsTests :: Spec
fiveRandsTests = do
    let exp = 8681089573064486461641871805074254223660
    it "gets the right answer" $ do
        product fiveRands `shouldBe` exp

randString3Test :: Spec
randString3Test = do
    let exp = "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"
        ans = showDigest . sha256 . BLU.fromString $ randString3
    it "gets the right answer" $ do
        exp `shouldBe` ans

generalATests :: Spec
generalATests = do
    let exp = 189908109902700
    let f g = fst $ g $ mkSeed 1
    let xs = map f [randEven, randOdd, randTen]
    it "gets the right answer" $ do
        product xs `shouldBe` exp

randPairTests :: Spec
randPairTests = do
    let exp = ('l', 282475249)
    let ans = fst $ randPair $ mkSeed 1
    it "gets the right answer" $ do
        ans `shouldBe` exp

randPairTests2 :: Spec
randPairTests2 = do
    let exp = ('l', 282475249)
    let ans = fst $ generalB (,) randLetter rand $ mkSeed 1
    it "gets the right answer" $ do
        ans `shouldBe` exp

main :: IO ()
main = hspec $ do
    fiveRandsTests
    randString3Test
    generalATests
    randPairTests
    randPairTests2
