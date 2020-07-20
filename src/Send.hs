module Send (
  sendExpr,
  sendString,
  sendSValue
  ) where

import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.IntMap.Lazy as IntMap
import Network.HTTP.Simple
import System.Process (readProcess)

import Message (Prim (Cons, Nil, Num), Expr (Ap, Prim))
import Modulate (modulate_, demodulate)
import qualified NFEval
import SValue

sendSValue :: SValue -> IO SValue
sendSValue val = do
  e <- sendExpr $ svToExpr val
  return $! svFromNFValue $ NFEval.reduceNF' IntMap.empty e -- XXX

sendExpr :: Expr -> IO Expr
sendExpr e = do
  me <- either (fail . ("runSend: request: modulate: " ++)) return $ modulate_ e
  rbody <- sendString me
  either (fail . ("runSend: response: demodulate: " ++)) return $ demodulate rbody

_exampleSend0 :: IO Expr
_exampleSend0 =
  sendExpr (Ap (Ap (Prim Cons) (Prim $ Num 0)) (Prim Nil))

useCurl :: Bool
useCurl = False

sendString :: String -> IO String
sendString me = do
  putStrLn $ "send: req: " ++ me
  let url = "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=a52c864b55954e25adc32abf69bc22b9"

  rbody <-
    if useCurl then do
      rbody <- readProcess
               "curl"
               ["-H", "accept: */*",
                "-H", "Content-Type: text/plain",
                "-d", me, "-s",
                url]
               ""
      return rbody
    else do
      -- see https://github.com/icfpcontest2020/starterkit-haskell/blob/master/app/Main.hs
      request' <- parseRequest ("POST " ++ url)
      let request = setRequestBodyLBS (BLU.fromString me) request'
      response <- httpLBS request
      let statuscode = show (getResponseStatusCode response)
      case statuscode of
          "200" -> return ()
          _ -> error ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
      return $ BLU.toString $ getResponseBody response

  putStrLn $ "send: res: " ++ rbody
  return rbody
