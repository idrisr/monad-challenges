{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module List where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs (x : xs) ys = helper x ys ++ allPairs xs ys
  where
    helper :: a -> [b] -> [(a, b)]
    helper x (y : ys) = (x, y) : helper x ys
    helper _ [] = []
allPairs [] _ = []
