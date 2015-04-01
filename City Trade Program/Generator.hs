module Generator (
pickFromFile,
rndSelectN,
rndSelect,
genNameN
) where

import Control.Monad.State
import Control.Monad.Random
import System.Random
import System.Environment
import qualified Data.ByteString.Char8 as Str
import qualified Helpers as H

rndSelectN :: (MonadRandom m, Eq a) => [a] -> Int -> m [a]
rndSelectN []  _ = return []
rndSelectN _ 0 = return []
rndSelectN ys n = do
  rndIndex <- getRandomR (1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  xs' <- rndSelectN xs (n-1)
  return (x:xs')

rndSelect :: (MonadRandom m, Eq a) => [a] -> m a
rndSelect [] = fail "rndSelect: empty list"
rndSelect ys = do
  rndIndex <- getRandomR (1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  return x

rndSelectBS :: (MonadRandom m) => [Str.ByteString] -> m Str.ByteString
rndSelectBS [] = fail "rndSelectBS: empty list"
rndSelectBS ys = do
  rndIndex <- getRandomR(1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  return x

pickFromFile :: (MonadRandom m) => Str.ByteString -> m Str.ByteString
pickFromFile h = rndSelectBS (Str.lines h)

pickNFromFile :: (MonadRandom m) => Str.ByteString -> Int -> m [Str.ByteString]
pickNFromFile h n = rndSelectN (Str.lines h) n

genNameN :: String -> String -> Int -> IO [Str.ByteString]
genNameN nat gen n = do
  nameList <- H.chkData ("Names/" ++ nat ++ gen ++ ".dat") "Names loaded" "Names not found"
  (pickNFromFile nameList n)
  --Need to figure actual nationality, but that comes later


genNameOnce = do
  putStrLn "Opening People Data in Append"
  chk <- H.chkData "Data/People.dat" "People Data Loaded" "People Data not found"
  putStrLn "What Nationality do you wish to generate?"
  putStrLn "Options are: Anglo, Byz, Frank, Goth and Welsh"
  nat <- getLine
  putStrLn "What Gender person would you like?"
  putStrLn"Options are Male and Female"
  gen <- getLine
  nameList <- H.chkData ("/Names/" ++ nat ++ gen ++ ".dat") "Names loaded" "Names not found"
  newName <- (pickFromFile nameList)
  Str.appendFile "People.dat" newName
