module Main where

import qualified ListSpec as L
import qualified StateSpec as S
import qualified MaybeSpec as M
import Test.Hspec

main = mapM hspec a
    where a = L.specs ++  S.specs ++  M.specs
