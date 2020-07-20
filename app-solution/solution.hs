
import System.Environment (getArgs)

import GameRun (run)
import Commander.Further (further)

main :: IO ()
main = do
  args <- getArgs
  run further (args !! 0) (args !! 1)
