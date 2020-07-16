import System.Environment (getArgs)
import System.Process (readProcess)
-- import System.Exit (ExitCode (..))

run :: String -> String -> IO ()
run server playerKey = do
  let cmd = ("curl", ["-s", "-d", "playerKey=" ++ playerKey, server])
  out <- uncurry readProcess cmd ""
  putStrLn $ "Server response: " ++ out
  {-
  case ec of
    ExitSuccess   -> return ()
    ExitFailure c -> fail $
                     "command failed with code: " ++ show c ++
                     ", cmd = " ++ unwords (uncurry (:) cmd)
   -}

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
