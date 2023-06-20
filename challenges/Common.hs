module Common where

import MCPrelude
import Prelude ()

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
    show (Just a) = "Just " ++ show a
    show Nothing = "Nothing"

instance Eq a => Eq (Maybe a) where
    Just a == Just b = a == b
    Nothing == Nothing = True
    _ == _ = False

class Monad m where
    return :: a -> m a
    bind :: m a -> (a -> m b) -> m b

newtype Gen a = Gen {unGen :: Seed -> (a, Seed)}

instance Monad Maybe where
    return = Just
    bind Nothing _ = Nothing
    bind (Just x) f = f x

instance Monad [] where
    return x = [x]
    bind [] _ = []
    bind (x : xs) f = f x ++ (xs `bind` f)

instance Monad Gen where
    return x = Gen $ \s -> (,) x s
    (Gen x) `bind` y = Gen $ \s ->
        let (s1, a) = x s
         in unGen (y s1) a

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = x `bind` \x1 -> return $ f x1

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f x y = x `bind` \x1 -> y `bind` \y1 -> return $ f x1 y1

sequence :: Monad m => [m a] -> m [a]
{- hlint ignore: "Use foldr" -}
sequence (x : xs) = liftM2 (:) x $ sequence xs
sequence [] = return []

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

combine :: Monad m => m (m a) -> m a
combine x = x `bind` id

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f x y z =
    x `bind` \x1 ->
        y `bind` \y1 ->
            z `bind` \z1 ->
                return $ f x1 y1 z1

ap :: Monad m => m (a -> b) -> m a -> m b
ap f x = f `bind` \f1 -> x `bind` \x1 -> return (f1 x1)
