import System.Environment (getArgs)
import System.Process (rawSystem)
import System.Exit (ExitCode (..))

run :: String -> String -> IO ()
run server playerKey = do
  let cmd = ("curl", ["-s", server ++ "?playerKey=" ++ playerKey])
  ec <- uncurry rawSystem cmd
  case ec of
    ExitSuccess   -> return ()
    ExitFailure c -> fail $
                     "command failed with code: " ++ show c ++
                     ", cmd = " ++ unwords (uncurry (:) cmd)

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
