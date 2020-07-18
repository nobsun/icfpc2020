module Send (
  send,
  ) where

import System.Process (readProcess)

import Message (Prim (Cons, Nil, Num), Expr (Ap, Prim))
import Modulate (modulate_, demodulate)

send :: Expr -> IO Expr
send e = do
  me <- either (fail . ("runSend: request: modulate: " ++)) return $ modulate_ e
  rbody <- readProcess
           "curl"
           ["-H", "accept: */*",
            "-H", "Content-Type: text/plain",
            "-d", me, "-s",
            "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=a52c864b55954e25adc32abf69bc22b9"]
           ""
  either (fail . ("runSend: response: demodulate: " ++)) return $ demodulate rbody

_exampleSend0 :: IO Expr
_exampleSend0 =
  send (Ap (Ap (Prim Cons) (Prim $ Num 0)) (Prim Nil))
