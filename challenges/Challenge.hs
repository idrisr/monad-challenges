module Challenge where

import MCPrelude

fiveRands :: [Integer]
fiveRands  = [a, b, c, d, e]
    where
        s0      = mkSeed 1
        (a, s1) = rand s0
        (b, s2) = rand s1
        (c, s3) = rand s2
        (d, s4) = rand s3
        (e, _)  = rand s4

randLetter :: Seed -> (Char, Seed)
randLetter s = (toLetter i, s')
    where (i, s') = rand s

randString3 :: String
randString3 = [a, b, c]
    where
        s0      = mkSeed 1
        (a, s1) = randLetter s0
        (b, s2) = randLetter s1
        (c, _)  = randLetter s2
