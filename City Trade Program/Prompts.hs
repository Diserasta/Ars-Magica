module Prompts
(
  yesno
 ,mainPrompt
) where

import System.IO
import System.Exit

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

mainPrompt :: IO String
mainPrompt = do
  putStr $ "Please enter a command - h for help - q to quit"
  hFlush stdout
  str <-getLine
  case str of
    "c"       -> return cmdList
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
  "c -- this list of commands \n" ++
  "h -- help text for the current prompt \n" ++
  "q -- exit the program safely, flushing all buffers and writing to the world \n" ++
  "q! -- quit unsafely, not saving any data and discarding all changes. Notably, this throws exit code 05 for any program running this Agent to catch \n" ++
  "purge -- purge the world buffer, discarding all changes and reverting to the state the world was in when this Agent was initialised. You will be asked for confirmation. \n"
