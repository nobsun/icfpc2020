module Send (
  sendNF,
  sendExpr,
  send_,
  ) where

import System.Process (readProcess)

import Message (Prim (Cons, Nil, Num), Expr (Ap, Prim))
import Modulate (modulate_, demodulate)
import NFEval (NFValue, asExpr)

sendNF :: NFValue -> IO Expr
sendNF = sendExpr . asExpr

sendExpr :: Expr -> IO Expr
sendExpr e = do
  me <- either (fail . ("runSend: request: modulate: " ++)) return $ modulate_ e
  rbody <- send_ me
  either (fail . ("runSend: response: demodulate: " ++)) return $ demodulate rbody

_exampleSend0 :: IO Expr
_exampleSend0 =
  sendExpr (Ap (Ap (Prim Cons) (Prim $ Num 0)) (Prim Nil))

send_ :: String -> IO String
send_ me = do
  putStrLn $ "send: req: " ++ me
  rbody <- readProcess
           "curl"
           ["-H", "accept: */*",
            "-H", "Content-Type: text/plain",
            "-d", me, "-s",
            "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=a52c864b55954e25adc32abf69bc22b9"]
           ""
  putStrLn $ "send: res: " ++ rbody
  return rbody
