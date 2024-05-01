module Euler3 where

import Prelude

import JS.BigInt (BigInt, fromString)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

-- | Largest Prime Factor
-- |
-- | The prime factors of 13195 are 5, 7, 13 and 29.
-- | What is the largest prime factor of the number 600851475143 ?
-- |
-- | https://projecteuler.net/problem=3

zero :: BigInt
zero = unsafePartial fromJust (fromString "0")

bigInt :: String -> BigInt
bigInt = unsafePartial fromJust <<< fromString

largestPrimeFactor :: BigInt -> BigInt
largestPrimeFactor n = largestPrimeFactor' n (bigInt "2")

largestPrimeFactor' :: BigInt -> BigInt -> BigInt
largestPrimeFactor' n i =
  if i * i > n
    then n
  else if n `mod` i == zero
      then largestPrimeFactor' (n `div` i) i
      else largestPrimeFactor' n (i + (bigInt "1"))

answer :: Unit -> String
answer _ = show $ largestPrimeFactor (bigInt "600851475143")