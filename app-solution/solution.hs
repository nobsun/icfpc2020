import System.Environment (getArgs)
import System.IO (hClose, hGetLine)
import System.Process (runInteractiveProcess, waitForProcess)
import System.Exit (ExitCode (..), exitSuccess)

run :: String -> String -> IO ()
run server playerKey = do
  let tempFile = "/tmp/output.txt"
  (inpH, outH, errH, ph) <- runInteractiveProcess
                         "curl"
                         ["-s", "-o", tempFile,
                          "-w", "%{http_code}",
                          "-d", playerKey,
                          server]
                         Nothing
                         Nothing
  hClose inpH
  hClose errH

  ec <- waitForProcess ph
  case ec of
    ExitSuccess   -> return ()
    ExitFailure c -> do
      putStrLn $ "run error code: " ++ show c
      exitSuccess

  httpCode <- take 3 <$> hGetLine outH
  body <- readFile tempFile
  if httpCode /= "200"
    then do putStr $ unlines
              ["Unexpected server response:",
               "HTTP code: " ++ httpCode]
            putStr $ "Response body: " ++ body
    else    putStr $ "Server response: " ++ body

main :: IO ()
main = do
  args <- getArgs
  run (args !! 0) (args !! 1)
