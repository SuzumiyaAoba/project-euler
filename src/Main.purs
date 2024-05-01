module Main
  ( answers
  , main
  )
  where

import Prelude

import Data.Array (index)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Effect.Console (log)
import Euler1 as Euler1
import Euler2 as Euler2
import Euler3 as Euler3
import Euler4 as Euler4
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)

answers :: Array (Unit -> String)
answers = [ Euler1.answer
          , Euler2.answer
          , Euler3.answer
          , Euler4.answer ]
          
indexFromArgs :: Array String -> Maybe Int
indexFromArgs args = 
  (add (-1)) <$> ((index args 2) >>= fromString)

main :: Effect Unit
main = do
  args <- argv
  log $ (unsafePartial fromJust $ (indexFromArgs args) >>= (index answers)) unit
    
