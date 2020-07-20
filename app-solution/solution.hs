
import System.Environment (getArgs)

import GameRun (run)
import Commander.Breaking (breaking)

main :: IO ()
main = do
  args <- getArgs
  run breaking (args !! 0) (args !! 1)
