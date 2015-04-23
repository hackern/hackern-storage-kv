module Hackern.Storage.KV(
  mkKVStorage,
  insert,
  lookup
) where

import Prelude hiding (lookup)
import Data.Binary
import Control.Monad.State.Strict hiding (put, get)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Device.BlockDevice
import System.Device.Memory

data KVMap = KVMap { firstEmptyLocation :: Word64
                   , location :: Map.Map String Word64 } deriving (Show)
instance Binary KVMap where
  put (KVMap i m) = do put i
                       put m
  get = do i <- get :: Get Word64
           m <- get :: Get (Map.Map String Word64)
           return (KVMap i m)

mkKVStorage :: Monad m =>
               BlockDevice m -> m ()
mkKVStorage dev = do
  let e = KVMap 2 Map.empty
  bdWriteBlock dev 0 (BL.toStrict $ encode e)
  bdWriteBlock dev 2 (BL.toStrict $ encode "A")
  
lookup :: Monad m =>
          BlockDevice m -> String -> m String
lookup dev key = do
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let loc = getLocation bd key
  b <- bdReadBlock dev loc
  let bd = decode (BL.fromStrict b) :: String
  return bd
  where
    getLocation :: KVMap -> String -> Word64
    getLocation m k =
      case Map.lookup k $ location m of
      Just n -> n
      Nothing -> 0


insert :: Monad m =>
          BlockDevice m -> String -> String -> m()
insert dev key value = do
  let e = encode value
  let sz = fromIntegral $ BL.length e `div` 4096 + 1
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let f = firstEmptyLocation bd
  let n = Map.insert key f $ location bd
  let m = KVMap (f + sz)  n
  bdWriteBlock dev 0 (BL.toStrict $ encode m)
  bdWriteBlock dev f (BL.toStrict $ e)
