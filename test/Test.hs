import Hackern.Storage.KV

main :: IO ()
main = do
  writeDebugConsole "Hello\n\n"

  Just dev <- memDev

  mkKVStorage dev
  insert dev "a" "hah"
  insert dev "b" "hahah"
  insert dev "c" "hahahaha"
  bd <- lookup dev "a"
  writeDebugConsole $ bd ++ "\n"
  bd <- lookup dev "b"
  writeDebugConsole $ bd ++ "\n"
  bd <- lookup dev "c"
  writeDebugConsole $ bd ++ "\n"

  writeDebugConsole "\nFinished\n"
