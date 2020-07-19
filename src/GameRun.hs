module GameRun (
  run,
  ) where

import Game (RequestTag (..), makeRequest, decodeResponse)
import CurlCmd (gameSend)
import Message (Expr (Prim), Prim (Nil, Num), fromList, toList)
import Modulate (modulate, demodulate)

run :: String -> String -> IO ()
run server playerKeyStr = do
  playerKey <- readIO playerKeyStr
  putStrLn $ "playerKey: " ++ show playerKey
  let request_ = request server playerKey
  joinR   <- request_ JOIN  nil
  either print print $ decodeResponse joinR
  startR  <- request_ START (startParam (2, 3, 4, 5))
  listPrint "START response: " startR
  either print print $ decodeResponse startR
  cmdR    <- request_ COMMANDS nil
  either print print $ decodeResponse cmdR

listPrint :: String -> Expr -> IO ()
listPrint prefix =
    maybe (putStrLn "Nothing") (mapM_ pprint . zip [0 :: Int ..]) . toList
  where
    pprint (n, e) = putStrLn $ prefix ++ ": " ++ show n ++ ": " ++ show e

request :: String -> Int -> RequestTag -> Expr -> IO Expr
request server playerKey rtag dexpr = do
  let req = makeRequest playerKey rtag dexpr
  listPrint (show rtag ++ " request: ") req
  resp  <- gameSend server $ modulate req
  either (fail . ("request: fail to demodulate response: " ++)) return $ demodulate resp
  -- either fail return $ decodeResponse expr

startParam :: (Int, Int, Int, Int) -> Expr
startParam (n1, n2, n3, n4) =
  fromList $ map (Prim . Num) [n1, n2, n3, n4]

nil :: Expr
nil = Prim Nil
