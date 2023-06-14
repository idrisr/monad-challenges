module Main where

import MCPrelude

fiveRands :: [Integer]
fiveRands  = [a, b, c, d, e]
    where
        (a, s1) = rand $ mkSeed 1
        (b, s2) = rand s1
        (c, s3) = rand s2
        (d, s4) = rand s3
        (e, s5) = rand s4

main :: IO ()
main = print $ product fiveRands
