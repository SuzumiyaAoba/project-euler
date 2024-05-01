module Euler2 where

import Prelude

import Data.Foldable (sum)
import Data.Int (even)
import Data.Lazy (defer)
import Data.List.Lazy (List(..), Step(..), filter, takeWhile)
import Data.Ord (greaterThan)

fibs :: List Int
fibs = fibs' 0 1
  where
    fibs' :: Int -> Int -> List Int
    fibs' a b =
      List $ defer \_ ->
        Cons a $ fibs' b (a + b)

answer :: Unit -> String
answer _ = show $ sum $ takeWhile (greaterThan 4000000) (filter even fibs)