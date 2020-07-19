module GameRun (
  run,
  ) where

import Game (RequestTag (..), makeRequest, decodeResponse)
import CurlCmd (gameSend)
import Message (Expr (Prim), Prim (Nil, Num), fromList)
import Modulate (modulate, demodulate)

run :: String -> String -> IO ()
run server playerKeyStr = do
  playerKey <- readIO playerKeyStr :: IO Int
  let request_ = request server playerKey
  joinR   <- request_ JOIN  nil
  either print print $ decodeResponse joinR
  startR  <- request_ START (startParam (11, 12, 13, 14))
  either print print $ decodeResponse startR
  cmdR    <- request_ COMMANDS nil
  either print print $ decodeResponse cmdR

request :: String -> Int -> RequestTag -> Expr -> IO Expr
request server playerKey rtag dexpr = do
  let reqData = modulate $ makeRequest playerKey rtag dexpr
  resp  <- gameSend server reqData
  putStrLn $ show rtag ++ " commands response: " ++ resp
  either (fail . ("request: fail to demodulate response: " ++)) return $ demodulate resp
  -- either fail return $ decodeResponse expr

startParam :: (Int, Int, Int, Int) -> Expr
startParam (n1, n2, n3, n4) =
  fromList $ map (Prim . Num) [n1, n2, n3, n4]

nil :: Expr
nil = Prim Nil
