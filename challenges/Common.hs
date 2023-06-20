module Common where

import List
import MCPrelude
import Maybe
import Prelude ()

class Monad m where
    return :: a -> m a
    bind :: m a -> (a -> m b) -> m b

newtype Gen a = Gen {unGen :: Seed -> (Seed, a)}

instance Monad Maybe where
    return = Just
    bind Nothing _ = Nothing
    bind (Just x) f = f x

instance Monad [] where
    return x = [x]
    bind [] _ = []
    bind (x : xs) f = f x ++ (xs `bind` f)

instance Monad Gen where
    return x = Gen $ \s -> (,) s x
    (Gen x) `bind` y = Gen $ \s ->
        let (s1, a) = x s
         in unGen (y a) s1
