module Main where

import State (
    fiveRands,
    generalB,
    randEven,
    randLetter,
    randOdd,
    randPair,
    randString3,
    randTen,
    repRandom,
 )

import Maybe
import Prelude hiding (Just, Maybe, Nothing)

import Data.ByteString.Lazy.UTF8 as BLU
import Data.Digest.Pure.SHA (sha256, showDigest)
import MCPrelude (
    greekDataA,
    greekDataB,
    mkSeed,
    rand,
    salaries,
 )

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

repRandomTest :: Spec
repRandomTest = do
    let exp = fst $ repRandom (replicate 3 randLetter) $ mkSeed 1
    it "gets the same answer as randString3" $ do
        exp `shouldBe` randString3

queryGreekTest :: Spec
queryGreekTest = do
    it "gets the right answers" $ do
        queryGreek greekDataA "alpha" `shouldBe` Just 2.0
        queryGreek greekDataA "beta" `shouldBe` Nothing
        queryGreek greekDataA "gamma" `shouldBe` Just 3.3333333333333335
        queryGreek greekDataA "delta" `shouldBe` Nothing
        queryGreek greekDataA "zeta" `shouldBe` Nothing
        queryGreek greekDataB "rho" `shouldBe` Nothing
        queryGreek greekDataB "phi" `shouldBe` Just 0.24528301886792453
        queryGreek greekDataB "chi" `shouldBe` Just 9.095238095238095
        queryGreek greekDataB "psi" `shouldBe` Nothing
        queryGreek greekDataB "omega" `shouldBe` Just 24.0

queryGreekTest2 :: Spec
queryGreekTest2 = do
    it "gets the same answer as queryGreek" $ do
        queryGreek2 greekDataA "alpha" `shouldBe` queryGreek greekDataA "alpha"
        queryGreek2 greekDataA "beta" `shouldBe` queryGreek greekDataA "beta"
        queryGreek2 greekDataA "gamma" `shouldBe` queryGreek greekDataA "gamma"
        queryGreek2 greekDataA "delta" `shouldBe` queryGreek greekDataA "delta"
        queryGreek2 greekDataA "zeta" `shouldBe` queryGreek greekDataA "zeta"
        queryGreek2 greekDataB "rho" `shouldBe` queryGreek greekDataB "rho"
        queryGreek2 greekDataB "phi" `shouldBe` queryGreek greekDataB "phi"
        queryGreek2 greekDataB "chi" `shouldBe` queryGreek greekDataB "chi"
        queryGreek2 greekDataB "psi" `shouldBe` queryGreek greekDataB "psi"
        queryGreek2 greekDataB "omega" `shouldBe` queryGreek greekDataB "omega"

yLinkTest = do
    it "works" $ do
        yLink (+) (Just 10) (Just 10) `shouldBe` Just 20
        yLink (+) Nothing (Just 10) `shouldBe` Nothing
        yLink (+) (Just 10) Nothing `shouldBe` Nothing
        yLink (+) Nothing Nothing `shouldBe` Nothing

addSalariesTest = do
    it "get the right salary totals" $ do
        addSalaries salaries "alice" "bob" `shouldBe` Just 195000
        addSalaries salaries "bob" "alice" `shouldBe` Just 195000
        addSalaries salaries "eve" "alice" `shouldBe` Nothing
        addSalaries salaries "carol" "alice" `shouldBe` Just 190000
        addSalaries salaries "eve" "frank" `shouldBe` Nothing

tailSumTest = do
    it "gets the correct Maybe value" $ do
        tailSum [1 .. 10] `shouldBe` Just (sum [2 .. 10])
        tailSum [] `shouldBe` Nothing
        tailSum [123] `shouldBe` Just 0

tailProdTest = do
    it "gets the correct Maybe value" $ do
        tailProd [1 .. 10] `shouldBe` Just (product [2 .. 10])
        tailProd [123] `shouldBe` Just 1
        tailProd [] `shouldBe` Nothing

main :: IO ()
main = hspec $ do
    fiveRandsTests
    generalATests
    queryGreekTest
    queryGreekTest2
    randPairTests
    randPairTests2
    randString3Test
    repRandomTest
    yLinkTest
    addSalariesTest
    tailSumTest
    tailProdTest
