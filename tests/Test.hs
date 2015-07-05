import Prelude hiding (lookup)
import System.Device.Memory
import Hackern.Storage.KV
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

  insert dev "/value1" 1
  insert dev "/value2" 2

  assertEq (lookup dev "/value1") 1
  assertEq (lookup dev "/value2") 2

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
