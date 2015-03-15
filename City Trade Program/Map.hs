module Map
( Node
, Path
, Place
, dist
) where

data Node = Node  { x :: Double
                  , y :: Double
                  } deriving (Show, Ord, Eq)

pointx :: Node -> Double
pointx (Node x _) = x

pointy :: Node -> Double
pointy (Node _ y) = y

dist :: Node -> Node -> Double
dist a b = sqrt((pointx a - pointx b)^2 + (pointy a - pointy b)^2)

dist :: Path -> Path -> Double
dist (Path _ a _) (Path _ b _) = dist a b

--A node is two floats representing a position--

data Path = Path  { id :: String
                  , intersectors :: [Path]
                  , endpoints :: [Place]
                  } deriving (Show, Ord, Eq)
--A path has an identifier (which can be a name), a list of intersecting paths, and two or more endpoints
--aka Places

sectors :: Path -> [Path]
sectors (Path _ p _) = p

endpts :: Path -> [Place]
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

traverse :: Place -> Double
traverse (Place _ p _) = map dist p
