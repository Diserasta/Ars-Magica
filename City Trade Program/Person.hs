module Person (
  Person
) where

import qualified Helpers as H
import Map
import Goods
data Person = Person  { name :: String
                      , age :: Int
                      , home :: Place
                      , rel :: [(Person, String)]} deriving (Read, Show, Eq)
