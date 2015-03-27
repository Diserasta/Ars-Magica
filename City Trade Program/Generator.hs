module Generator (
pickFromFile,
rndSelectN,
rndSelect
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
