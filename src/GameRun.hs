module GameRun (
  run,
  ) where

import Game (RequestTag (..), makeRequest)
import CurlCmd (gameSend)
import Message (Expr (Prim), Prim (Nil, Num), fromList)
import Modulate (modulate)

run :: String -> String -> IO ()
run server playerKeyStr = do
  playerKey <- readIO playerKeyStr :: IO Int
  let request_ = request server playerKey
  joinR   <- request_ JOIN  nil
  putStrLn $ "join response: " ++ joinR
  startR  <- request_ START (startParam (11, 12, 13, 14))
  putStrLn $ "start response: " ++ startR
  cmdR    <- request_ COMMANDS nil
  putStrLn $ "commands response: " ++ cmdR

request :: String -> Int -> RequestTag -> Expr -> IO String
request server playerKey ptag dexpr = do
  let reqData = modulate $ makeRequest playerKey ptag dexpr
  gameSend server reqData

startParam :: (Int, Int, Int, Int) -> Expr
startParam (n1, n2, n3, n4) =
  fromList $ map (Prim . Num) [n1, n2, n3, n4]

nil :: Expr
nil = Prim Nil
