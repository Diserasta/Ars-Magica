import Data.Char
import Data.List
import Control.Monad
import Control.Concurrent
import System.Environment
import System.Directory
import qualified Data.ByteString as Str
import qualified Data.ByteString.Lazy as LStr
import Data.Time.Clock
import Goods
import Generator
import Money
import Map
import Prompts
import qualified Helpers as H

genLoop :: Int -> IO [()]
genLoop n = replicateM n gen

gen = do
  --Redo this with ByteString
  putStrLn "Opening People Data in Append"
  chk <- chkData "Data/People.dat" "People Data Loaded" "People Data not found"
  putStrLn "What Nationality do you wish to generate?"
  putStrLn "Options are: Anglo, Byz, Frank, Goth and Welsh"
  nat <- getLine
  putStrLn "What Gender person would you like?"
  putStrLn"Options are Male and Female"
  gen <- getLine
  nameList <- chkData ("/Names/" ++ nat ++ gen ++ ".dat") "Names loaded" "Names not found"
  newName <- (pickFromFile nameList)
  Str.appendFile "People.dat" newName

initWorld = do
  putStrLn "Agent Started..."
  s <- getArgs
  putStrLn "Initialising World"
  --Let's try reading in the map first
  putStrLn "Opening the Map"
  chk <- chkData "Map.dat" "Map Acquired" "Map not found"
   --Move Everything From the File into Memory
  putStrLn "Map Constructed"
  putStrLn "Populating World with Merchants and Goods"
  --Read from some more files
  putStrLn "World Initialised"
  putStrLn "Checking World Time"
  let time = "12:00"
  putStrLn ("World Time is:" ++ time)
  putStrLn "World is Synchronised"
  putStrLn "Agent Awaiting Input..."

cmdLoop = do
  cmd <- mainPrompt
  putStrLn cmd
  cmdLoop

main = do
  initWorld
  cmdLoop
