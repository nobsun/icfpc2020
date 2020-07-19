module Send (
  sendNF,
  sendExpr,
  send_,
  ) where

import System.Process (readProcess)

import Message (Prim (Cons, Nil, Num), Expr (Ap, Prim))
import Modulate (modulate_, demodulate)
import Eval (NFValue (NFPAp))

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

asExpr :: NFValue -> Expr
asExpr (NFPAp p@(Num _) [])    = Prim p
asExpr (NFPAp p@Nil [])        = Prim p
asExpr (NFPAp Cons [n1, n2])   = Ap (Ap (Prim Cons) (asExpr n1)) (asExpr n2)
asExpr  nfv                    = error $ "asExpr: not NF or cannot convert to expr: " ++ show nfv

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
