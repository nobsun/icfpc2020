module Game (
  RequestTag (..),
  create,
  encodeRequest,
  Command (..), cmdShipId, encodeCommand,

  ResponseTag (..),
  GameStage (..),
  ShipRole (..), oppositeRole,
  ShipInfo,
  GameState,
  Response,
  decodeResponse,
  decodeResponse_,
  ) where

import Message (Expr (Ap, Prim), Prim (Num, Cons), num, cons, fromList, toList)

data RequestTag
  = CREATE
  | JOIN
  | START
  | COMMANDS
  deriving (Eq, Show)

requestCode :: RequestTag -> Int
requestCode =
    code
  where
    code CREATE     = 1
    code JOIN       = 2
    code START      = 3
    code COMMANDS   = 4

create :: Expr
create =
  fromList [ num $ requestCode CREATE ]

encodeRequest :: Int -> RequestTag -> Expr -> Expr
encodeRequest playerKey rtag dataExpr =
  fromList [ num $ requestCode rtag, num playerKey, dataExpr ]


data Command
  = Accelerate Int (Int, Int)
  | Detonate   Int
  | Shoot      Int (Int, Int) Expr
  deriving Show

cmdShipId :: Command -> Int
cmdShipId (Accelerate x _) = x
cmdShipId (Detonate x)     = x
cmdShipId (Shoot x _ _)    = x

encodeCommand :: Command -> Expr
encodeCommand =
    dispatch
  where
    dispatch (Accelerate sid (ax, ay))     = fromList [ num 0, num sid, cons (num ax) (num ay) ]
    dispatch (Detonate   sid)              = fromList [ num 1, num sid ]
    dispatch (Shoot      sid (px, py) x3)  = fromList [ num 2, num sid, cons (num px) (num py), x3 ]

data ResponseTag
  = WRONG_REQUEST
  | GAME_STAGE
  deriving (Eq, Show)

decodeResponseCode :: Int -> Maybe ResponseTag
decodeResponseCode =
    dispatch
  where
    dispatch 0 = Just WRONG_REQUEST
    dispatch 1 = Just GAME_STAGE
    dispatch _ = Nothing

data GameStage
  = NotYetStarted
  | AlreadyStarted
  | Finished
  deriving (Eq, Show)

decodeGameStage :: Int -> Maybe GameStage
decodeGameStage =
    dispatch
  where
    dispatch 0 = Just NotYetStarted
    dispatch 1 = Just AlreadyStarted
    dispatch 2 = Just Finished
    dispatch _ = Nothing

data ShipRole
  = Attacker
  | Defender
  deriving (Eq, Show)

decodeShipRole :: Int -> Maybe ShipRole
decodeShipRole =
    dispatch
  where
    dispatch 0 = Just Attacker
    dispatch 1 = Just Defender
    dispatch _ = Nothing

oppositeRole :: ShipRole -> ShipRole
oppositeRole =
    opp
  where
    opp Attacker = Defender
    opp Defender = Attacker

decodeStaticInfo :: Expr -> Either String (Expr, ShipRole, Expr, Expr, Expr)
decodeStaticInfo x = do
  let raise = Left . ("decodeStaticInfo: " ++)
  es     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x
  case es of
    x0 : Prim (Num rc) : x2 : x3 : x4 : _ -> do
      role <- maybe (raise $ "unknown player-role code: " ++ show rc) return $ decodeShipRole rc
      return (x0, role, x2, x3, x4)
    _                                     ->
      raise $ "unknown static-info expression: " ++ show es

type ShipInfo = (ShipRole, Int, (Int, Int), (Int, Int))

decodeShipAndCommand :: Expr -> Either String ((ShipInfo, (Expr, Expr, Expr, Expr)), [Expr])
decodeShipAndCommand sce = do
  let raise = Left . ("decodeShipAndCommand: " ++)
  sc <- maybe (raise $ "failed to convert to list: " ++ show sce) return $ toList sce
  (shipe, cmdse) <- case sc of
    s : c : _ ->  return (s, c)
    _         ->  raise $ "unknown ship-and-commands expression: " ++ show sc

  ship <- maybe (raise $ "failed to convert ship to list: " ++ show shipe) return $ toList shipe
  (role, shipId, pos, vel, other) <- case ship of
    (Prim (Num rc) : Prim (Num shipId) :
     Ap (Ap (Prim Cons) (Prim (Num px))) (Prim (Num py)) :
     Ap (Ap (Prim Cons) (Prim (Num vx))) (Prim (Num vy)) :
     x4 : x5 : x6 : x7 : _) -> do
      role <- maybe (raise $ "unknown player-role code: " ++ show rc) return $ decodeShipRole rc
      return (role, shipId, (px, py), (vx, vy), (x4, x5, x6, x7))
    _                       ->
      raise $ "unknown ship expression: " ++ show ship

  cmds <- maybe (raise $ "failed to convert commands to list: " ++ show cmdse) return $ toList cmdse
  return (((role, shipId, pos, vel), other), cmds)


decodeGameState :: Expr -> Either String ((Expr, Expr), [(ShipInfo, [Expr])])
decodeGameState x = do
  let raise = Left . ("decodeGameState: " ++)
  es     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x

  (gtick, x1, scps) <- case es of
    gtick : x1 : scsExpr : _ -> do
      sces <- maybe (raise $ "failed to convert ships-and-commands to list: " ++ show x) return $ toList scsExpr
      scs <- mapM decodeShipAndCommand sces
      let ps = [ (ship, cmds)
               | ((ship, _x4567), cmds) <- scs]
      return (gtick, x1, ps)
    _                            ->
      raise $ "unknown game-state expression: " ++ show es

  return ((gtick, x1), scps)

type GameState = (Expr, [(ShipInfo, [Expr])])

type Response  = (GameStage, ShipRole, GameState)

decodeResponse_ :: Expr -> Either String (ResponseTag, Maybe Response)
decodeResponse_ x = do
  let raise = Left . ("decodeResponse: " ++)
  ees     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x
  (rcode, rexprs) <- case ees of
    []               ->  raise $ "error. response list is nil."
    Prim (Num c):es  ->  return (c, es)
    e:_              ->  raise $ "unknown response expression: " ++ show e
  t <- maybe (raise $ "unknown response code: " ++ show rcode) return $ decodeResponseCode rcode
  body <- case t of
            WRONG_REQUEST                            -> return Nothing
            GAME_STAGE    -> case rexprs of
              Prim (Num stc) : static : state : _ -> do
                gst <- maybe (raise $ "unkown game-stage code: " ++ show stc) return $ decodeGameStage stc
                (_x0, role, _x2, _x3, _x4) <- decodeStaticInfo static
                ((gtick, _x1), shipAndCmds) <- decodeGameState state
                return $ Just (gst, role, (gtick, shipAndCmds))
              _                                   ->
                raise $ "unknown gameStage response: " ++ show rexprs

  return (t, body)

decodeResponse :: Expr -> Either String Response
decodeResponse x = do
  (t, body) <- decodeResponse_ x
  case t of
    WRONG_REQUEST -> Left "decodeResponse: error. request was wrong."
    GAME_STAGE    -> maybe (Left "decodeResponse: should not be happen") return body
