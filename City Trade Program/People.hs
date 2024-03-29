module People (
Person(..)
) where

import qualified Helpers as H
import Map
import Goods
data Person = Person  { name :: String
                      , age :: Int
                      , gender :: String
                      , home :: Place
                      , rel :: [(Person, String)]} deriving (Read, Show, Eq)
