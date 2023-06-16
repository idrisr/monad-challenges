module Maybe where

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

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_ : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((b, c) : xs) =
    if a == b
        then Just c
        else lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y =
    if y == 0
        then Nothing
        else Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x : xs) = case maximumMay xs of
    Just a -> Just $ max a x
    Nothing -> Nothing

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x : xs) = case minimumMay xs of
    Just a -> Just $ min a x
    Nothing -> Nothing

queryGreek :: GreekData -> String -> Maybe Double
queryGreek g s = p
  where
    xs = lookupMay s g
    {- HLINT ignore "Replace case with maybe" -}
    m = case xs of
        Just a -> tailMay a
        Nothing -> Nothing
    n = case m of
        Just a -> maximumMay a
        Nothing -> Nothing
    o = case xs of
        Just a -> headMay a
        Nothing -> Nothing
    p = case (o, n) of
        (Just a, Just b) -> divMay (fromIntegral b) (fromIntegral a)
        (Just _, Nothing) -> Nothing
        (Nothing, Just _) -> Nothing
        (Nothing, Nothing) -> Nothing

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just a) = f a
chain _ Nothing = Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 g s = q
  where
    xs = lookupMay s g
    m = xs `link` tailMay
    n = m `link` maximumMay
    o = xs `link` headMay
    p = n `link` \a -> o `link` \b -> Just (fromIntegral a, fromIntegral b)
    q = p `link` uncurry divMay

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries s a b = yLink (+) (lookupMay a s) (lookupMay b s)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f x y = x `link` \x1 -> y `link` \y1 -> mkMaybe $ f x1 y1

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd xs = transMaybe product $ tailMay xs

tailSum :: Num a => [a] -> Maybe a
tailSum xs = transMaybe sum $ tailMay xs

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay $ tailMay xs

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = transMaybe minimumMay $ tailMay xs

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f x = x `link` \a -> Just $ f a

combine :: Maybe (Maybe a) -> Maybe a
combine x = x `link` id
