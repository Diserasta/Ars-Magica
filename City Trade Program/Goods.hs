module Goods
( Item
, Good
) where

import Money

data Item = Item  {name :: String
                  ,qty :: Double
                  ,quality :: Int} deriving (Show)
--Name, Quality and Quantity--
data Good = Good  {good :: Item
                  ,cost :: LbP} deriving (Show)
--Item, Cost


