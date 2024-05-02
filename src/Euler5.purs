module Euler5 where

import Prelude

import Data.Array (foldl, (..))

-- | Smallest Multiple
-- | 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- |
-- | What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-- |
-- | https://projecteuler.net/problem=5

gcd :: Int -> Int -> Int
gcd a b = 
  if b == 0 then 
    a
  else
    gcd b (a `mod` b)

lcm :: Int -> Int -> Int
lcm a b = 
  (a `div` (gcd a b)) * b

smallestMultiple :: Int -> Int
smallestMultiple n = foldl lcm 1 (1 .. n)

answer :: Unit -> String
answer _ = show $ smallestMultiple 20