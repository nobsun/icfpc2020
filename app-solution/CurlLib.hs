{-# LANGUAGE TypeApplications #-}

module CurlLib (run) where

import Network.Curl
  (CurlOption (CurlPostFields, CurlPostFieldSize), CurlResponse_ (respStatus, respBody),
   method_POST, initialize, do_curl_)

run :: String -> String -> IO ()
run server playerKey = do
  curl <- initialize
  let params = playerKey
      curlOpts =
        method_POST ++
        [CurlPostFields [params], CurlPostFieldSize $ fromIntegral $ length params]
  resp <- do_curl_ @[(String, String)] curl server curlOpts
  let status = respStatus resp
      body = respBody resp
  if status /= 200
    then do putStr $ unlines
              ["Unexpected server response:",
                "HTTP code: " ++ show status]
            putStr $ "Response body: " ++ body
    else    putStr $ "Server response: " ++ body
