{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module List where

import MCPrelude
import Common

data Card = Card Int String

instance Show Card where
    show (Card i s) = show i ++ s

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 = liftM4

combStep :: [a -> b] -> [a] -> [b]
combStep = ap

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2
