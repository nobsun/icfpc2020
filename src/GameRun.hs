module GameRun (
  run,
  ) where

import Game
  (RequestTag (..), encodeRequest, decodeResponse, decodeResponse_,
   Command (Shoot, Accelerate), encodeCommand,
   GameStage (..), ShipInfo, ShipRole, oppositeRole, )
import CurlCmd (gameSend)
import Message (Expr, num, nil, fromList, toList)
import Modulate (modulate, demodulate)

run :: String -> String -> IO ()
run server playerKeyStr = do
  playerKey <- readIO playerKeyStr
  putStrLn $ "playerKey: " ++ show playerKey
  let request_ = request server playerKey

  joinR   <- request_ JOIN  nil
  listPrint "JOIN response: " joinR
  either print print $ decodeResponse joinR
  startR  <- request_ START (startParam (2, 3, 4, 5))
  listPrint "START response: " startR

  case decodeResponse startR of
    Left e     ->  do
      putStrLn $ "somethind wrong: " ++ e
      nullLoop request_
    Right (_stage, myRole, (_tick, ships))  ->
      commandLoop request_ myRole ships


commandLoop :: (RequestTag -> Expr -> IO Expr)
            -> ShipRole
            -> [(ShipInfo, [Expr])]
            -> IO ()
commandLoop request_ myRole iships =
    loop (0 :: Int) iships
  where
    enemyRole = oppositeRole myRole
    loop n ships = do
      let putLn = putStrLn . ((show n ++ ": ") ++)
          myShips =
            [ (shipId, pos, vel)
            | ((role, shipId, pos, vel), _) <- ships
            , role == myRole ]
          enemis =
            [ (pos, vel)
            | ((role, _, pos, vel), _) <- ships
            , role == enemyRole ]

          firstTarget = take 1 enemis

          _shootCommands =
            [ Shoot shipId (epos <+> evel) nil
            | (shipId, _, _) <- myShips
            , (epos, evel) <- firstTarget ]

          -- 敵の集団から遠ざかる加速度
          furtherAcc pos vel =
            foldr (<+>) (0,0)
            [ vsignum (npos <-> (ePos <+> eVel))
            | (ePos, eVel) <- enemis
            , let npos = pos <+> vel
                  _npos = pos <+> vel `vquot` 2]

          commands =
            [ Accelerate shipId (furtherAcc pos vel)
            | (shipId, pos, vel) <- myShips ]

      putLn $ "my-role: " ++ show myRole
      putLn $ "enemy-role: " ++ show enemyRole
      putLn $ "my-ships: " ++ show myShips
      mapM_ (putLn . ("command: " ++) . show) commands
      cmdR    <- request_ COMMANDS $ fromList $ map encodeCommand $ commands
      listPrint "COMMANDS response: " cmdR
      let recover em = do
            putStrLn $ "response decode error: " ++ em
            putStrLn "recovering using previous state..."
            return (AlreadyStarted, ships)
          response (_tag, mayResp) =
            maybe
            (recover "wrong request error.")
            (\ (stage, _, (_tick, ships1)) -> return (stage, ships1))
            mayResp
      res@(stage, ships1) <- either recover response $ decodeResponse_ cmdR
      putLn $ "response: " ++ show res
      case stage of
        NotYetStarted   -> loop (n+1) ships1
        AlreadyStarted  -> loop (n+1) ships1
        Finished        -> return ()

    (x, y) `vquot` n = (x `quot` n, y `quot` n)

    vneg (x, y) = (-x, -y)
    vsignum (x, y) = (signum x, signum y)

    (x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

    p <-> q = p <+> vneg q

    infixl 6 <+>, <->
    infixl 7 `vquot`



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
