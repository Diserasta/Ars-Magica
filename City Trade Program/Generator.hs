{-# LANGUAGE ScopedTypeVariables #-}

module Generator (
pickFromFile,
rndSelectN,
rndSelect,
genNameN,
genUniform,
genNormal
) where

import Control.Monad.State
import qualified Control.Monad.Random as CMR
import Data.Word (Word32)
import Data.Random
import Data.RVar
import System.Random
import System.Environment
import People
import Map
import qualified Data.ByteString.Char8 as Str
import qualified Helpers as H

--Returns a random number from 1 to 100, uniform distribution
genUniform :: forall m. (CMR.MonadRandom m) => m Int
genUniform = do
  r <- runRVar (uniform 0 100) (CMR.getRandom :: m Word32)
  return r

--Generate a normally distributed number with mean m and stddev s
genNormal :: forall m. (CMR.MonadRandom m) => Double -> Double ->  m Double
genNormal m s = do
  r <- runRVar (normal m s) (CMR.getRandom :: m Word32)
  return r

--Generate a log-normally distributed number with mean m and stddev s^2
genLogNormal :: forall m. (CMR.MonadRandom m) => Double -> Double -> m Double
genLogNormal mu sigmaSq = do
  r <- runRVar (normal mu sigmaSq) (CMR.getRandom :: m Word32)
  return (exp r)

--Pick N random members from a list of type a
rndSelectN :: (CMR.MonadRandom m, Eq a) => [a] -> Int -> m [a]
rndSelectN []  _ = return []
rndSelectN _ 0 = return []
rndSelectN ys n = do
  rndIndex <- CMR.getRandomR (1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  xs' <- rndSelectN xs (n-1)
  return (x:xs')

--Pick a random member from a list of type a
rndSelect :: (CMR.MonadRandom m, Eq a) => [a] -> m a
rndSelect [] = fail "rndSelect: empty list"
rndSelect ys = do
  rndIndex <- CMR.getRandomR (1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  return x

--Single implementation of rndSelect for ByteStrings, for easy IO
rndSelectBS :: (CMR.MonadRandom m) => [Str.ByteString] -> m Str.ByteString
rndSelectBS [] = fail "rndSelectBS: empty list"
rndSelectBS ys = do
  rndIndex <- CMR.getRandomR(1, length ys)
  let (x, xs) = H.removeAt rndIndex ys
  return x

--Helper function to pick a line from a file
pickFromFile :: (CMR.MonadRandom m) => Str.ByteString -> m Str.ByteString
pickFromFile h = rndSelectBS (Str.lines h)

pickNFromFile :: (CMR.MonadRandom m) => Str.ByteString -> Int -> m [Str.ByteString]
pickNFromFile h n = rndSelectN (Str.lines h) n

--Name Generator
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
  nameList <- H.chkData ("Names/" ++ nat ++ gen ++ ".dat") "Names loaded" "Names not found"
  newName <- (pickFromFile nameList)
  Str.appendFile "People.dat" newName

--Age Generator
--This one should be easier. Smaller range.

genAgeN :: (CMR.MonadRandom m) => Double -> Int -> m [Double]
genAgeN _ 0 = return []
genAgeN x n = replicateM n (genWeightedAge x)

--This one is trickier
--We want to generate an age that's weighted around a certain value
--But for pickFromList, we'd need rationals representing weights for
--each and every member, which is kinda painful
--I mean, we could zip up the values together
--Below is the calling function
--I settled on log-normally distributed values with sigmaSq 0.125
genWeightedAge :: (CMR.MonadRandom m) => Double -> m Double
genWeightedAge x = do
  r <- genLogNormal 0 0.125
  return (r * x)

--Hometown Generator
--Take the list of all places?
--Then choose one?
genPlace :: (CMR.MonadRandom m) => [Place] -> m Place
genPlace x = do
  r <- rndSelect x
  return r
genPlaceN :: (CMR.MonadRandom m) => [Place] -> Int -> m [Place]
genPlaceN x n = do
  r <- rndSelectN x n
  return r

--Now for the Person Generator
--Takes in a Gender and NameType, as well as expected age, and hometown
genPerson :: String -> String -> Int -> [Place] -> [(Person, String)] -> IO Person
genPerson nameloc gen expAge home rels = do
  n <- (genNameN nameloc gen 1)
  let k = Str.unpack (head n)
  a <- (genWeightedAge (fromIntegral expAge))
  let b = floor a
  h <- genPlace home
  return (Person k b gen h rels)

