module Goods
( Item
, Good
) where

import Money

data Amount = Weight  {ton :: Int
                      ,oz :: Int}
            | Length  {len :: Int
                      ,wid :: Int} deriving (Show, Eq)

len2wgt :: Amount -> Int -> Amount
len2wgt (Length x y) r = (Weight {ton = res, oz = modu})
  where amt = (x * y) `quot` r
        res = amt `quot` 2420
        modu = amt `rem` 2420

wgt2len :: Amount -> Int -> Int -> Amount
wgt2len (Weight x y) ratio w = (Length {len = res, wid = w})
  where amt = (x * 2420) + y
        res = amt * ratio

data Item = Item  {name :: String
                  ,qty :: Amount
                  ,quality :: Int} deriving (Read, Show, Eq)
--Name, Quality and Quantity--
data Good = Good  {good :: Item
                  ,cost :: Money} deriving (Read, Show, Eq)
--Item, Cost


