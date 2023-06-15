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
