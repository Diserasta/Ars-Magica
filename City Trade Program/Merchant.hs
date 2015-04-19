module Merchant (
  Merchant
) where

import People
import Goods
data Merchant = Merchant  { basic :: Person
                          , inv :: [Good] } deriving (Eq, Read, Show)
