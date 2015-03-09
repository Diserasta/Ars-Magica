import Data.Char
import Data.List
import Data.String

module Types.Money where

data LbP = LbP  {pounds :: Int
                ,pence :: Int} deriving (Show,Eq)
module Types.Goods where
import Types.Money
data Item = Item  {name :: String
                  ,qty :: Double
                  ,quality :: Int} deriving (Show)
--Name, Quality and Quantity--
data Good = Good  {good :: Item
                  ,cost :: LbP} deriving (Show)
--Item, Cost

data Node = Node Float Float deriving (Show)
--A node is two floats representing a position--
data Settlement = Settlement String Node [Good]
--A settlement is a name, a node, and a list of good--

