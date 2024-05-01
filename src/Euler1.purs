module Euler1 where

import Prelude

import Data.List (List, range, filter)
import Data.Foldable (sum)

-- | Multiples of 3 and 5
-- |
-- | If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
-- | The sum of these multiples is 23.
-- |
-- | Find the sum of all the multiples of 3 or 5 below 1000.
-- |
-- | https://projecteuler.net/problem=1

ns :: List Int
ns = range 0 999

multiples :: List Int
multiples = filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) ns

answer :: Unit -> String
answer _ = show $ sum multiples