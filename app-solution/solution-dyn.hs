
import System.Environment (getArgs)

import qualified CurlLib as Lib
import qualified CurlCmd as Cmd

useLib :: Bool
useLib = True


{-
  libcurl からリンクされている krb5 ライブラリの
  *.a イメージが Debian では提供されていなかったので
  static link できなかった
 -}
run :: String -> String -> IO ()
run
  | useLib     =  Lib.run
  | otherwise  =  Cmd.run

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
