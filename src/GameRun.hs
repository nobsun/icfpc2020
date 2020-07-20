module GameRun (
  run,
  ) where

import Game
  (RequestTag (..), encodeRequest, decodeResponse, decodeResponse_,
   Command (Shoot, Accelerate), encodeCommand,
   StageTag (..), StaticInfo (..), GameState (..), ShipInfo (..), oppositeRole, )
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
    Left e                                  ->  do
      putStrLn $ "somethind wrong: " ++ e
      nullLoop request_
    Right (Finished, _, _)                  ->
      return ()
    Right (_stage, static, gstate)  ->
      commandLoop request_ static gstate


commandLoop :: (RequestTag -> Expr -> IO Expr)
            -> StaticInfo  -- ^ static informations in one game
            -> GameState   -- ^ game-state at start
            -> IO ()
commandLoop request_ staticInfo igstate =
    loop (0 :: Int) igstate
  where
    myRole = staticPlayerRole staticInfo
    enemyRole = oppositeRole myRole
    loop n gstate = do
      let putLn = putStrLn . ((show n ++ ": ") ++)
          ships = gstateShips gstate
          myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
          enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

          firstTarget = take 1 enemyShips

          _shootCommands =
            [ Shoot (shipId ship) (shipPos enemy <+> shipVel enemy) nil
            | ship <- myShips
            , enemy <- firstTarget ]

          -- Accelerate 命令の仕様で、加速度ベクトルは逆向きに与える.
          -- 敵の集団に近づく加速度
          _closerAcc pos vel =
            foldr (<+>) (0,0)
            [ vsignum (npos <-> (shipPos enemy <+> shipVel enemy))
            | enemy <- enemyShips
            , let npos = pos <+> vel
                  _npos = pos <+> vel `vquot` 2]

          -- Accelerate 命令の仕様で、加速度ベクトルは逆向きに与える.
          -- 敵の集団から遠ざかる加速度
          furtherAcc pos vel =
            foldr (<+>) (0,0)
            [ vsignum ((shipPos enemy <+> shipVel enemy) <-> npos)
            | enemy <- enemyShips
            , let npos = pos <+> vel
                  _npos = pos <+> vel `vquot` 2]

          commands =
            [ Accelerate (shipId ship) (furtherAcc (shipPos ship) (shipVel ship))
            | ship <- myShips ]

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
            (\ (stage, _, gstate1) -> return (stage, gstate1))
            mayResp
      res@(stage, gstate1) <- either recover response $ decodeResponse_ cmdR
      putLn $ "response: " ++ show res
      case stage of
        NotYetStarted   -> loop (n+1) gstate1
        AlreadyStarted  -> loop (n+1) gstate1
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
