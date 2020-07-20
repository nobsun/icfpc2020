
import System.Environment (getArgs)

import GameRun (run)
import Commander.Shooter (shooter)

main :: IO ()
main = do
  args <- getArgs
  run shooter (args !! 0) (args !! 1)
