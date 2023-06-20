{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module List where

import MCPrelude

data Card = Card Int String

instance Show Card where
    show (Card i s) = show i ++ s

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f (x : xs) ys zs = allCombs (f x) ys zs ++ allCombs3 f xs ys zs
allCombs3 _ [] _ _ = []

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f (x : xs) ys zs ws = allCombs3 (f x) ys zs ws ++ allCombs4 f xs ys zs ws
allCombs4 _ [] _ _ _ = []

combStep :: [a -> b] -> [a] -> [b]
combStep (f:fs) xs = map f xs ++ combStep fs xs
combStep [] _ = []

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (x:xs) ys = combStep [f x] ys ++ allCombs f xs ys
allCombs _ _ [] = []
allCombs _ [] _ = []