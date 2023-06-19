{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module List where

import MCPrelude

data Card = Card Int String

instance Show Card where
    show (Card i s) = show i ++ s

allPairs :: [a] -> [b] -> [(a, b)]
allPairs (x : xs) ys = helper x ys ++ allPairs xs ys
  where
    helper :: a -> [b] -> [(a, b)]
    helper x (y : ys) = (x, y) : helper x ys
    helper _ [] = []
allPairs [] _ = []

allCards :: [Int] -> [String] -> [Card]
allCards xs ys = map (uncurry Card) $ allPairs xs ys
