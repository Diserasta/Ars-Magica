module Goods
( Item
, Good
) where

import Money

data Item = Item  {name :: String
                  ,qty :: Double
                  ,quality :: Int} deriving (Show, Eq)
--Name, Quality and Quantity--
data Good = Good  {good :: Item
                  ,cost :: Money} deriving (Show, Eq)
--Item, Cost


