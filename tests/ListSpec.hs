module ListSpec where

import List (
    allCards,
    allCombs3,
    allPairs,
    combStep,
 )

import MCPrelude
import Test.Hspec

allPairsTest = do
    describe "does the cartesian product" $ do
        it "three list" $ do
            allPairs [1, 2, 3] [4, 5, 6]
                `shouldBe` [ (1, 4)
                           , (1, 5)
                           , (1, 6)
                           , (2, 4)
                           , (2, 5)
                           , (2, 6)
                           , (3, 4)
                           , (3, 5)
                           , (3, 6)
                           ]
        it "two list" $ do
            allPairs [1, 2] [3, 4]
                `shouldBe` [ (1, 3)
                           , (1, 4)
                           , (2, 3)
                           , (2, 4)
                           ]

allCombs3Test = do
    describe "gets the answer from the website" $ do
        it "three list" $ do
            let exp =
                    [ (1, 3, 5)
                    , (1, 3, 6)
                    , (1, 4, 5)
                    , (1, 4, 6)
                    , (2, 3, 5)
                    , (2, 3, 6)
                    , (2, 4, 5)
                    , (2, 4, 6)
                    ]
            let ans = allCombs3 (,,) [1, 2] [3, 4] [5, 6]
             in exp `shouldBe` ans

combStepTest = do
    describe "wtf" $ do
        it "wtf" $ do
            combStep [(+1), (*3)] [3, 4] `shouldBe` [4, 5, 9, 12]

specs = [allPairsTest, allCombs3Test, combStepTest]
