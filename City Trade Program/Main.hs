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

initWorld = do
  putStrLn "Agent Started..."
  s <- getArgs
  putStrLn "Initialising World"
  --Let's try reading in the map first
  putStrLn "Opening the Map"
  chk <- H.chkData "Map.dat" "Map Acquired" "Map not found"
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
