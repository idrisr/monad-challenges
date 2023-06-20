module MaybeSpec where

import Maybe
import Common
import Prelude hiding (Just, Maybe, Nothing)

import MCPrelude (
    greekDataA,
    greekDataB,
    mkSeed,
    rand,
    salaries,
 )

import Test.Hspec

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

specs =
    [ queryGreekTest
    , queryGreekTest2
    , yLinkTest
    , addSalariesTest
    , tailSumTest
    , tailProdTest
    ]
