
import System.Environment (getArgs)

import CurlCmd (run)

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
