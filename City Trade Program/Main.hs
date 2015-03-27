import Data.Char
import Data.List
import Control.Monad
import Control.Concurrent
import System.Environment
import qualified Data.ByteString as Str
import qualified Data.ByeString.Lazy as LStr
import Goods
import Generator
import Money
import Map
import qualified Helpers as H

genLoop = do
  --Redo this with ByteString
  putStrLn "Opening People Data in Append"
  peopleList <- openFile "People.dat" AppendMode
  putStrLn "People Data Loaded"
  putStrLn "What Nationality do you wish to generate?"
  nat <- getLine
  putStrLn "Options are: Anglo, Byz, Frank, Goth and Welsh"
  gen <- getLine
  nameList <- openFile (nat ++ gen ++ ".dat") ReadMode
  namec <- hGetContents nameList
  let names = lines namec
  --Test hPutStr
  hPutStr peopleList (pickFromFile namec)


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
