module State where

import MCPrelude
import Prelude ()
import Common

fiveRands :: [Integer]
fiveRands = fst $ unGen a $ mkSeed 1
    {- HLINT ignore "Use replicateM" -}
    where a = sequence $ replicate 5 $ Gen rand

randString3 :: String
randString3 = fst $ unGen a $ mkSeed 1
    where a = sequence $ replicate 3 randLetter

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

randLetter :: Gen Char
randLetter = liftM toLetter $ Gen rand

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 = liftM2

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = liftM2 (,)

randEven :: Gen Integer
randEven = liftM (* 2) $ Gen rand

randOdd :: Gen Integer
randOdd = liftM (+ 1) randEven

randTen :: Gen Integer
randTen = liftM (* 5) randEven

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter $ Gen rand

repRandom' :: [Gen a] -> Gen [a]
repRandom' = sequence

mkGen :: a -> Gen a
mkGen  = return

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

generalA :: (a -> b) -> Gen a -> Gen b
generalA = liftM
