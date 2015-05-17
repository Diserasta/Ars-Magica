module Map
( Node(..)
, Path(..)
, Place(..)
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

--A node is two floats representing a position--

data Path = Path  { id :: String
                  , cost :: Double
                  , endpoints :: (Place, Place)
                  } deriving (Read, Show, Ord, Eq)
--A path has an identifier (which can be a name), a list of intersecting paths,
--and two or more endpoints
--aka Places

endpts :: Path -> (Place, Place)
endpts (Path _ _ pl) = pl

data Place = Place  { name :: String
                    , pos :: Node
                    , paths :: [Path]
                    } deriving (Read, Show, Ord, Eq)
--A place has a name, a position, and a list of paths that connect to it

placePos :: Place -> Node
placePos (Place _ pos _) = pos

placePaths :: Place -> [Path]
placePaths (Place _ _ paths) = paths

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

