module Euler1 where

import Prelude

import Data.List (List, range, filter)
import Data.Foldable (sum)

ns :: List Int
ns = range 0 999

multiples :: List Int
multiples = filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) ns

answer :: Unit -> String
answer _ = show $ sum multiples