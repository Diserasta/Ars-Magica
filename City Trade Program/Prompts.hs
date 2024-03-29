module Prompts
(
  yesno
 ,mainPrompt
) where

import Control.Monad.State
import System.IO
import System.Directory
import System.Exit
import Data.List.Split
import qualified Data.ByteString.Char8 as Str

import Generator
import Map
yesno :: String -> IO Bool
yesno prompt = do
  putStr $ prompt ++ " y/n: "
  hFlush stdout
  str <- getLine
  case str of
    "y" -> return True
    "n" -> return False
    _   -> do
      putStrLn "Invalid Input."
      yesno prompt

addPrompt :: IO String
addPrompt = do
  putStr $ "What would you like to generate?\n"
  putStr $ "Options are: p for places, q to return"
  hFlush stdout
  str <- getLine
  case str of
    "p"   -> addPlaceCmd
    "q"   -> return ""
    _     -> do
      putStrLn "Invalid Input. "
      addPrompt

genPrompt :: IO String
genPrompt = do
  putStr $ "What would you like to generate?\n"
  putStr $ "Options are: n for names, p for people, q to return"
  hFlush stdout
  str <- getLine
  case str of
    "n"   -> genNameCmd
    "p"   -> return ""--genPerCmd
    "q"   -> return ""
    _     -> do
      putStrLn "Invalid Input."
      genPrompt
--Gen Person command goes here. It should call Generator's Functions
--But that's work for tomorrow - It's like 1AM
--It's now July. WORK ETHICS :D
mainPrompt :: IO String
mainPrompt = do
  putStr $ "Please enter a command - h for help - q to quit\n"
  hFlush stdout
  str <-getLine
  case str of
    "a"       -> addPrompt
    "c"       -> return cmdList
    "g"       -> genPrompt
    "h"       -> return mainHelpText
    "purge"   -> return ""
    "q"       -> exitWith ExitSuccess
    "q!"      -> exitWith (ExitFailure 05)
    _         -> do
      putStrLn "Invalid Input."
      mainPrompt

mainHelpText :: String
mainHelpText =
  "This is the main prompt for the world. It can used to monitor, edit, or simulate the world. \n" ++
  "The list of commands can be accessed by typing c at the main prompt. \n" ++
  ""

cmdList :: String
cmdList =
  "a -- Add an object into the world \n" ++
  "c -- this list of commands \n" ++
  "g -- generate something \n" ++
  "h -- help text for the current prompt \n" ++
  "q -- exit the program safely, flushing all buffers and writing to the world \n" ++
  "q! -- quit unsafely, not saving any data and discarding all changes. Notably, this throws exit code 05 for any program running this Agent to catch \n" ++
  "purge -- purge the world buffer, discarding all changes and reverting to the state the world was in when this Agent was initialised. You will be asked for confirmation. \n"

--Write to Map
addPlace :: Place ->  IO ()
addPlace plc = do
  Str.appendFile "Map.dat" (Str.pack (show plc))



--Add Commands
addPlaceCmd :: IO String
addPlaceCmd = do
  putStrLn "What Type of place would you like to add?"
  putStrLn "Options are: Region, Biome, Settlement, District, Building, Room or Feature"
  ptype <- getLine
  putStrLn ("Please enter the co-ordinates of the " ++ show ptype ++ " separated by a comma")
  inn <- getLine
  let cc = map (splitOn " ") (splitOn "," inn)
  putStrLn ("Please enter the name of the " ++ show ptype)
  namen <- getLine
  putStrLn "Please enter the list of connected path ids separated by a space"
  pathsl' <- getLine
  let pathsl = splitOn " " pathsl'
  let finalp = Place namen (Node (read (head (head cc)) :: Double) (read (last (head cc)) :: Double), Node (read (head (last cc)) :: Double) (read (last (last cc)) :: Double)) (read ptype :: PlaceType) (map read pathsl :: [Int])
  putStrLn ("Success." ++ show finalp)
  chk <- yesno "Do you wish to commit this place?"
  if chk then do
    ret <- addPlace finalp
    return ("Committed!" ++ show ret)
  else do
    return ("Not Commited!")


  


--Generate Commands
genNameCmd :: IO String
genNameCmd = do
  putStrLn "How many names would you like to generate?"
  num <- getLine
  let n = read (num) :: Int
  putStrLn "What Nationality do you wish to generate?"
  putStrLn "Options are: Anglo, Byz, Frank, Goth and Welsh"
  nat <- getLine
  putStrLn "What Gender person would you like?"
  putStrLn "Options are Male and Female"
  gen <- getLine
  nameList <- pickNameN nat gen n
  mapM_ Str.putStrLn nameList
  return ("Success. " ++ show n ++ " Names Generated.")

genNameLoop :: Int  -> String -> String -> IO [Str.ByteString]
genNameLoop n nat gen = (pickNameN nat gen n)


--genPersonCmd :: IO String
--genPersonCmd = do
--  putStrLn "How many people would you like to generate?"
--  num <- getLine
--  let n = read (num) :: Int
--  putStrLn "What Nationality do you wish to generate?"
--  putStrLn "Options are: Anglo, Byz, Frank, Goth and Welsh"
--  nat <- getLine
--  putStrLn "What Gender would you like to generate?"
--  putStrLn "Options are Male and Female"
--  gen <-getLine
--  
