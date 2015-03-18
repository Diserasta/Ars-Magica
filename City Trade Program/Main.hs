import Goods
import Money
import Map
import qualified Helpers as H
import Data.Char
import Data.List
import Control.Monad
import Control.Concurrent
import System.Environment

main = do
  putStrLn "Agent Started..."
  [s] <- getArgs
  putStrLn "Initialising World"
  --Let's try reading in the map first
  putStrLn "Opening the Map"
  nodeMap <- openFile "Map.txt" ReadWriteMode
  mapc <- hGetContents nodeMap
  putStrLn "Map Acquired"
  putStrLn "Constructing Map in Memory..."
  --Move Everything From the File into Memory
  putStrLn "Map Constructed"
  putStrLn "Populating World with Merchants and Goods"
  --Read from some more files
  putStrLn "World Initialised"
  putStrLn "Checking World Time"
  let time = "TEST"
  putStrLn "World Time is:" ++ time
  putStrLn "World is Synchronised"
  putStrLn "Agent Awaiting Input..."
