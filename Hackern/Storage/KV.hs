module Hackern.Storage.KV(
  mkKVStorage,
  insert,
  lookup,
  load,
  store,
  delete
) where

import Prelude hiding (lookup)
import Data.Binary
import Control.Monad.State.Strict hiding (put, get)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Device.BlockDevice
import System.Device.Memory

data Item = Value { val :: Word64 } |
            Pointer { pos :: Word64, len :: Word64 } deriving (Show, Eq)
instance Binary Item where
  put (Value i) = do put (0 :: Word8)
                     put i
  put (Pointer len pos) = do put (1 :: Word8)
                             put len
                             put pos
  get = do t <- get :: Get Word8
           case t of
             0 -> do i <- get
                     return (Value i)
             1 -> do l <- get
                     p <- get
                     return (Pointer l p)
instance Ord Item where
  (Pointer a _) `compare` (Pointer b _) = a `compare` b
  _ `compare` _ = 0 `compare` 0

data KVMap = KVMap { size :: Word64
                   , kvmap :: Map.Map String Item } deriving (Show)
instance Binary KVMap where
  put (KVMap i m) = do put i
                       put m
  get = do i <- get
           m <- get
           return (KVMap i m)

mkKVStorage :: Monad m =>
               BlockDevice m -> Word64 -> m ()
mkKVStorage dev sz = do
  let e = KVMap (sz - 4) Map.empty
  bdWriteBlock dev 0 (BL.toStrict $ encode e)

lookup :: Monad m =>
          BlockDevice m -> String -> m Word64
lookup dev key = do
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let val = getValue bd key
  return val
  where
    getValue :: KVMap -> String -> Word64
    getValue m k =
      case Map.lookup k $ kvmap m of
      Just a -> val a
      Nothing -> 0
               
insert :: Monad m =>
          BlockDevice m -> String -> Word64 -> m()
insert dev key val = do
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let n = Map.insert key (Value val) $ kvmap bd
  let m = KVMap (size bd) n
  bdWriteBlock dev 0 (BL.toStrict $ encode m)
               
load :: Monad m =>
        BlockDevice m -> String -> m String
load dev key = do
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let loc = getLocation bd key
  b <- bdReadBlock dev loc
  let bd = decode (BL.fromStrict b) :: String
  return bd
  where
    getLocation :: KVMap -> String -> Word64
    getLocation m k =
      case Map.lookup k $ kvmap m of
      Just a -> pos a
      Nothing -> 0

store :: Monad m =>
         BlockDevice m -> String -> String -> m()
store dev key value = do
  let e = encode value
  let sz = fromIntegral $ BL.length e `div` 4096 + 1
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let f = pos $ largestEmptyBlock bd
  let n = Map.insert key (Pointer f sz) $ kvmap bd
  let m = KVMap (size bd) n
  bdWriteBlock dev 0 (BL.toStrict $ encode m)
  bdWriteBlock dev f (BL.toStrict $ e)

delete :: Monad m =>
          BlockDevice m -> String -> m()
delete dev key = do
  b <- bdReadBlock dev 0
  let bd = decode (BL.fromStrict b) :: KVMap
  let m = KVMap (size bd) (Map.delete key $ kvmap bd)
  bdWriteBlock dev 0 (BL.toStrict $ encode m)

largestEmptyBlock :: KVMap -> Item
largestEmptyBlock m = leb 4 (size m) (sorted (Map.elems (kvmap m)))
  where
    leb :: Word64 -> Word64 -> [Item] -> Item
    leb i size [] = Pointer i (size - i)
    leb i size m = minb (Pointer i ((pos (head m)) - i)) (leb ((pos (head m)) + (len (head m))) size (tail m))
    sorted m = List.sort $ filter isPointer m
    isPointer :: Item -> Bool
    isPointer m = case m of
                    Pointer _ _ -> True
                    Value _ -> False
    minb (Pointer a b) (Pointer c d)
      | b < d = Pointer c d
      | otherwise = Pointer a b
