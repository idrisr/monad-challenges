module Challenge where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands  = [a, b, c, d, e]
    where
        s0      = mkSeed 1
        (a, s1) = rand s0
        (b, s2) = rand s1
        (c, s3) = rand s2
        (d, s4) = rand s3
        (e, _)  = rand s4

randLetter :: Gen Char
randLetter s = (toLetter i, s')
    where (i, s') = rand s

randString3 :: String
randString3 = [a, b, c]
    where
        s0      = mkSeed 1
        (a, s1) = randLetter s0
        (b, s2) = randLetter s1
        (c, _)  = randLetter s2

generalA :: (a->b) -> Gen a -> Gen b
generalA f x s = let (b, s') = x s in (f b, s')

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*5) randEven
