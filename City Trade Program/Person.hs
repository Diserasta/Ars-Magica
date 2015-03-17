module Person (

) where

import qualified Helpers as H
import Goods
import Map
Data Person = Person  { name :: String
                      , age :: Int
                      , home :: Place
                      , rel :: [(Person, String)]} deriving (Show, Eq)
