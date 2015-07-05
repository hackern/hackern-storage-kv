import Prelude hiding (lookup)
import System.Device.Memory
import Hackern.Storage.KV
import System.Exit (exitFailure, exitSuccess)

memDev = newMemoryBlockDevice 4096 1048576

assert x = if x then exitSuccess else exitFailure

main :: IO ()
main = do
  Just dev <- memDev
  mkKVStorage dev 1048576

  insert dev "/value1" 1
  insert dev "/value2" 2
  bd <- lookup dev "/value1"
  assert $ bd == 1
  -- store dev "a" "hah"
  -- store dev "b" "hahah"
  -- store dev "c" "hahahaha"
  -- bd <- load dev "a"
  -- print bd
  -- bd <- load dev "b"
  -- print bd
  -- bd <- load dev "c"
  -- print bd
  -- delete dev "b"
  -- store dev "d" "hohoh"
  -- bd <- load dev "d"
  -- print bd

  -- print "\nFinish"
