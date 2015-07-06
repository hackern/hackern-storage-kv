import Prelude hiding (lookup)
import System.Device.Memory
import Hackern.Storage.KV
import Data.Word(Word64(..))
import System.Exit (exitFailure, exitSuccess)

memDev = newMemoryBlockDevice 4096 1048576

assertEq e v = do
  v' <- e
  if v == v' then return ()
  else exitFailure

main :: IO ()
main = do
  Just dev <- memDev
  mkKVStorage dev 1048576

  store dev "/value1" (1 :: Word64)
  store dev "/value2" (2 :: Word64)

  assertEq (load dev "/value1") (1 :: Word64)
  assertEq (load dev "/value2") (2 :: Word64)

  store dev "a" "hah"
  store dev "b" "hahah"
  store dev "c" "hahahaha"

  assertEq (load dev "a") "hah"
  assertEq (load dev "b") "hahah"
  assertEq (load dev "c") "hahahaha"

  delete dev "b"
  store dev "d" "hohoh"
  assertEq (load dev "d") "hohoh"

  exitSuccess
