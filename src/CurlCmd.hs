module CurlCmd (run) where

import System.IO (hClose, hGetLine)
import System.Process (runInteractiveProcess, waitForProcess)
import System.Exit (ExitCode (..))

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
  putStrLn $ "ServerUrl: " ++ server ++ "; PlayerKey: " ++ playerKey
  case ec of
    ExitFailure c ->
      putStrLn $ "run error code: " ++ show c
    ExitSuccess   -> do
      statusStr <- take 3 <$> hGetLine outH
      body <- readFile tempFile
      if statusStr /= "200"
        then do putStr $ unlines
                  ["Unexpected server response:",
                   "HTTP code: " ++ statusStr]
                putStr $ "Response body: " ++ body
        else    putStr $ "Server response: " ++ body
