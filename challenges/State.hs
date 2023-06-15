module State where

import MCPrelude
import Prelude()

fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
  where
    s0 = mkSeed 1
    (a, s1) = rand s0
    (b, s2) = rand s1
    (c, s3) = rand s2
    (d, s4) = rand s3
    (e, _) = rand s4

randLetter :: Gen Char
randLetter s = (toLetter i, s')
  where
    (i, s') = rand s

randString3 :: String
randString3 = [a, b, c]
  where
    s0 = mkSeed 1
    (a, s1) = randLetter s0
    (b, s2) = randLetter s1
    (c, _) = randLetter s2

generalA :: (a -> b) -> Gen a -> Gen b
generalA f x s = let (b, s') = x s in (f b, s')

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f x y s = (f a b, s2)
  where
    (a, s1) = x s
    (b, s2) = y s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair x y s = ((a, b), s2)
  where
    (a, s1) = x s
    (b, s2) = y s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

randEven :: Gen Integer
randEven = generalA (* 2) rand

randOdd :: Gen Integer
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer
randTen = generalA (* 5) randEven

randPair :: Gen (Char, Integer)
randPair s =
    let (a, s1) = randLetter s
        (b, s2) = rand s1
     in ((a, b), s2)

type Gen a = Seed -> (a, Seed)

-- repRandom :: [Gen a] -> Seed -> (a, Seed)
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (x : xs) s = (y : ys, s1)
  where
    (y, s1) = x s
    (ys, s2) = repRandom xs s1

mkGen :: a -> Gen a
mkGen a s = (a, s)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo x y s = (b, s2)
  where
    (a, s1) = x s
    (b, s2) = y a s1
