module GameRun (
  run,
  ) where

import Game
  (Commander, encodeCommand,
   RequestTag (..), encodeRequest, decodeResponse, decodeResponse_,
   StageTag (..), StaticInfo (..), GameState (..), ShipInfo (..), oppositeRole, )
import CurlCmd (gameSend)
import Message (Expr, num, nil, fromList, toList)
import Modulate (modulate, demodulate)

run :: Commander -> String -> String -> IO ()
run commander server playerKeyStr = do
  playerKey <- readIO playerKeyStr
  putStrLn $ "playerKey: " ++ show playerKey
  let request_ = request server playerKey

  joinR   <- request_ JOIN  nil
  listPrint "JOIN response: " joinR
  either print print $ decodeResponse joinR
  startR  <- request_ START (startParam (2, 3, 4, 5))
  listPrint "START response: " startR

  case decodeResponse startR of
    Left e                                  ->  do
      putStrLn $ "somethind wrong: " ++ e
      nullLoop request_
    Right (Finished, _, _)                  ->
      return ()
    Right (stage, static, gstate)  ->
      commandLoop request_ commander stage static gstate


commandLoop :: (RequestTag -> Expr -> IO Expr)
            -> Commander
            -> StageTag
            -> StaticInfo  -- ^ static informations in one game
            -> GameState   -- ^ game-state at start
            -> IO ()
commandLoop request_ commander istage staticInfo igstate =
    loop (0 :: Int) istage igstate
  where
    myRole = staticPlayerRole staticInfo
    enemyRole = oppositeRole myRole
    loop n stage gstate = do
      let putLn = putStrLn . ((show n ++ ": ") ++)
          ships = gstateShips gstate
          myShips    = [ ship | ship <- ships, shipRole ship == myRole ]

      commands <- commander stage staticInfo igstate

      putLn $ "my-role: " ++ show myRole
      putLn $ "enemy-role: " ++ show enemyRole
      putLn $ "my-ships: " ++ show myShips
      mapM_ (putLn . ("command: " ++) . show) commands
      cmdR    <- request_ COMMANDS $ fromList $ map encodeCommand $ commands
      listPrint "COMMANDS response: " cmdR
      let recover em = do
            putStrLn $ "response decode error: " ++ em
            putStrLn "recovering using previous state..."
            return (AlreadyStarted, gstate)
          response (_tag, mayResp) =
            maybe
            (recover "wrong request error.")
            (\ (stage1, _, gstate1) -> return (stage1, gstate1))
            mayResp
      res@(stage1, gstate1) <- either recover response $ decodeResponse_ cmdR
      putLn $ "response: " ++ show res
      case stage of
        NotYetStarted   -> loop (n+1) stage1 gstate1
        AlreadyStarted  -> loop (n+1) stage1 gstate1
        Finished        -> return ()


nullLoop :: (RequestTag -> Expr -> IO Expr) -> IO ()
nullLoop request_ =
    loop
  where
    loop = do
      cmdR    <- request_ COMMANDS nil
      res@(stage, _, _) <- either fail return $ decodeResponse cmdR
      putStrLn $ "nloop: " ++ show res
      case stage of
        NotYetStarted   -> loop
        AlreadyStarted  -> loop
        Finished        -> return ()

listPrint :: String -> Expr -> IO ()
listPrint prefix =
    maybe (putStrLn "Nothing") (mapM_ pprint . zip [0 :: Int ..]) . toList
  where
    pprint (n, e) = putStrLn $ prefix ++ ": " ++ show n ++ ": " ++ show e

request :: String -> Int -> RequestTag -> Expr -> IO Expr
request server playerKey rtag dexpr = do
  let req = encodeRequest playerKey rtag dexpr
  listPrint (show rtag ++ " request: ") req
  resp  <- gameSend server $ modulate req
  either (fail . ("request: fail to demodulate response: " ++)) return $ demodulate resp
  -- either fail return $ decodeResponse expr

startParam :: (Int, Int, Int, Int) -> Expr
startParam (n1, n2, n3, n4) =
  fromList $ map num [n1, n2, n3, n4]
