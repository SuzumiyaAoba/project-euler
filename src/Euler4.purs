module Euler4 where

import Prelude

import Data.Array (filter, (..))
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Maybe (Maybe, fromJust)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- | A palindromic number reads the same both ways. The largest palindrome made
-- | from the product of two 2-digit numbers is 9009 = 91 × 99.
-- |
-- | Find the largest palindrome made from the product of two 3-digit numbers.

reverse :: String -> String
reverse = toCharArray >>> Array.reverse >>> fromCharArray

isPalindrome :: Int -> Boolean
isPalindrome n = let s = show n in s == reverse s

product :: Tuple Int Int -> Int
product (Tuple x y) = x * y

largestPalindrome :: Maybe Int
largestPalindrome = 
  maximum $ map product $ filter (\(Tuple x y) -> isPalindrome (x * y)) 
    do
      x <- 100 .. 999
      y <- 100 .. x
      [Tuple x y]

answer :: Unit -> String
answer _ = show $ unsafePartial fromJust largestPalindrome