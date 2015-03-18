module Merchant (
  Merchant
) where

import Person
import Goods
data Merchant = Merchant  { basic :: Person
                          , inv :: [Good] } deriving (Eq, Read, Show)
