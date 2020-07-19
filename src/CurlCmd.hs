module CurlCmd (
  run,
  gameSend,
  ) where

import Control.Monad (unless)
import System.IO (hClose, hGetLine)
import System.Process (runInteractiveProcess, waitForProcess)
import System.Exit (ExitCode (..))
import System.Posix.Process (getProcessID)

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

gameSend :: String -> String -> IO String
gameSend server dataString = do
  pid <- getProcessID
  let tempFile = "/tmp/send-output-" ++ show pid ++ ".txt"

  (inpH, outH, errH, ph) <- runInteractiveProcess
                            "curl"
                            ["-s", "-o", tempFile,
                             "-w", "%{http_code}",
                             "-d", dataString,
                             server ++ "/aliens/send"]
                            Nothing
                            Nothing
  hClose inpH
  hClose errH

  ec <- waitForProcess ph
  case ec of
    ExitFailure c -> do
      let err = "curl: run error code: " ++ show c
      putStrLn err
      fail err
    ExitSuccess   -> do
      statusStr <- take 3 <$> hGetLine outH
      body <- readFile tempFile

      unless (statusStr == "200") $ do
        let err = unlines
                  ["Unexpected server response:",
                   "HTTP code: " ++ statusStr,
                   "Response body: " ++ body]
        putStr err
        fail err

      return body
