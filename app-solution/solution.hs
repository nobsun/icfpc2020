
import System.Environment (getArgs)

import GameRun (run)

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
