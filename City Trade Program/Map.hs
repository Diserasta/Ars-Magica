module Map
( Node
, Path
, Place
, distN
, followPath
) where

import Data.List

data Node = Node  { x :: Double
                  , y :: Double
                  } deriving (Show, Ord, Eq)

pointx :: Node -> Double
pointx (Node x _) = x

pointy :: Node -> Double
pointy (Node _ y) = y

distN :: Node -> Node -> Double
distN a b = sqrt((pointx a - pointx b)^2 + (pointy a - pointy b)^2)

--A node is two floats representing a position--

data Path = Path  { id :: String
                  , cost :: Int
                  , endpoints :: (Place, Place)
                  } deriving (Show, Ord, Eq)
--A path has an identifier (which can be a name), a list of intersecting paths, and two or more endpoints
--aka Places

endpts :: Path -> (Place, Place)
endpts (Path _ _ pl) = pl

data Place = Place  { name :: String
                    , pos :: Node
                    , paths :: [Path]
                    } deriving (Show, Ord, Eq)
--A place has a name, a position, and a list of paths that connect to it

placePos :: Place -> Node
placePos (Place _ pos _) = pos

placePaths :: Place -> [Path]
placePaths (Place _ _ paths) = paths

pathfImpl :: Eq a => [a] -> a -> a -> [(a,a)] -> [[a]]
pathfImpl trail src dest clauses
  | src == dest = [src:trail]
  | otherwise = do
    let (nexts, rest) = partition ((==src) . fst) clauses
    next <- nexts
    pathfImpl (src:trail) (snd next) dest rest

followPath :: Eq a => a -> a -> [(a,a)] -> [[a]]
followPath src dest clauses = map reverse (pathfImpl [] src dest clauses)
