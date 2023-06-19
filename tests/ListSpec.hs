module ListSpec where

import List (
    allPairs,
 )

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

specs = [ allPairsTest ]
