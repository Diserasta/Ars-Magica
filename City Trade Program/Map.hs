module Map
( Node(..)
, Path(..)
, Place(..)
, PlaceType(..)
, distN
, followPath
) where

import Data.List
import qualified Helpers as H

data Node = Node  { x :: Double
                  , y :: Double
                  } deriving (Read, Show, Ord, Eq)

pointx :: Node -> Double
pointx (Node x _) = x

pointy :: Node -> Double
pointy (Node _ y) = y

distN :: Node -> Node -> Double
distN a b = sqrt((pointx a - pointx b)^2 + (pointy a - pointy b)^2)

--A type list for our places

data PlaceType =  Region | Country | Biome | Settlement | District | Building | Room | Feature
                deriving (Read, Show, Ord, Eq)

--A node is two floats representing a position--

data Path = Path  { id :: Int
                  , cost :: Double
                  , endpts :: (String, String) --Listed by identifier
                  } deriving (Read, Show, Ord, Eq)
--A path has an identifier (which can be a name), a list of intersecting paths,
--and two or more endpoints
--aka Places
--TODO:
--  Traverse places that don't have paths

data Place = Place  { name :: String
                    , pos :: (Node, Node)
                    , pval :: PlaceType
                    , contains :: [Place]
                    , paths :: [Int] --Listed by identifier
                    } deriving (Read, Show, Ord, Eq)
--A place has a name, a position, and a list of paths that connect to it

--Data Type for our complete map
data Carta = Carta { loc :: [Place]
                    } deriving (Read, Show, Ord, Eq)

--Technical implementation of a path-follower
pathfImpl :: Eq a => ([a],Double) -> a -> a -> [(a,a,Double)] -> [([a], Double)]
pathfImpl (trail, cost) src dest clauses
  | src == dest = [(src:trail, cost)]
  | otherwise = do
    let (nexts, rest) = partition ((==src) . H.get1st) clauses
    next <- nexts
    pathfImpl ((src:trail), cost + H.get3rd next) (H.get2nd next) dest rest

--Reverse all the directions as the path-follwer prepends to the trail so it can run in constant time
sanPath :: ([a], Double) -> ([a], Double)
sanPath (list, cost) = (reverse list, cost)

--Taking a source and destination point, as well as a list of links (and their costs)
--return a list of all acyclic paths from src to dest and their costs
followPath :: Eq a => a -> a -> [(a,a,Double)] -> [([a], Double)]
followPath src dest clauses = map sanPath (pathfImpl ([],0.0) src dest clauses)


--Finally, I need a convenient way to add, or delete Nodes, Paths etc
--In that regard, I also need a data structure for this
--addPath :: Int -> Double -> (String, String) -> Path

