module GameRun (
  run,
  ) where

import Game
  (RequestTag (..), encodeRequest, decodeResponse,
   Command (Shoot), encodeCommand,
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
    loop iships
  where
    enemyRole = oppositeRole myRole
    loop ships = do
      let myShips =
            [ shipId
            | ((role, shipId, _, _), _) <- ships
            , role == myRole ]
          enemyTargets =
            [ pos <+> vel
            | ((role, _, pos, vel), _) <- ships
            , role == enemyRole ]
          firstTarget = take 1 enemyTargets

          commands =
            [ encodeCommand $ Shoot shipId target nil
            | shipId <- myShips
            , target <- firstTarget ]

      cmdR    <- request_ COMMANDS $ fromList commands
      (stage, _, (_tick, ships1)) <- either fail return $ decodeResponse cmdR
      case stage of
        NotYetStarted   -> loop ships1
        AlreadyStarted  -> loop ships1
        Finished        -> return ()

    (px, py) <+> (vx, vy) = (px + vx, py + vy)

nullLoop :: MonadFail m => (RequestTag -> Expr -> m Expr) -> m ()
nullLoop request_ =
    loop
  where
    loop = do
      cmdR    <- request_ COMMANDS nil
      (stage, _, _) <- either fail return $ decodeResponse cmdR
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
