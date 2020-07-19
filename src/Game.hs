module Game (
  RequestTag (..),
  create,
  makeRequest,

  ResponseTag (..),
  GameStage (..),
  ShipRole (..),
  Response,
  decodeResponse,
  decodeResponse_,
  ) where

import Message (Expr (Ap, Prim), Prim (Num, Cons), fromList, toList)

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
  fromList [ Prim $ Num $ requestCode CREATE ]

makeRequest :: Int -> RequestTag -> Expr -> Expr
makeRequest playerKey rtag dataExpr =
  fromList [ Prim $ Num $ requestCode rtag, Prim $ Num playerKey, dataExpr ]


{-
data Command
  = Accelerate Int (Int, Int)
  | Detonate   Int
  | Shoot      Int (Int, Int) Expr

shipId :: Command -> Int
shipId (Accelerate x _) = x
shipId (Detonate x)     = x
shipId (Shoot x _ _)    = x

-- encodeCommand :: Command
 -}

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

decodeGameState :: Expr -> Either String ((Expr, Expr), (ShipRole, Expr, (Int, Int), (Int, Int), (Expr, Expr, Expr, Expr)), [Expr])
decodeGameState x = do
  let raise = Left . ("decodeGameState: " ++)
  es     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x

  (gtick, x1, shipe, cmdse) <- case es of
    gtick : x1 : sce : _ -> do
      sc <- maybe (raise $ "failed to convert ships-and-commands to list: " ++ show x) return $ toList sce
      case sc of
        s : c : _  -> do
          return (gtick, x1, s, c)
        _          ->
          raise $ "unknown ships-and-command expression: " ++ show sc
    _                            ->
      raise $ "unknown game-state expression: " ++ show es

  ship <- maybe (raise $ "failed to convert ship to list: " ++ show x) return $ toList shipe
  (role, shipId, pos, vel, other) <- case ship of
    (Prim (Num rc) : shipId :
     Ap (Ap (Prim Cons) (Prim (Num px))) (Prim (Num py)) :
     Ap (Ap (Prim Cons) (Prim (Num vx))) (Prim (Num vy)) :
     x4 : x5 : x6 : x7 : _) -> do
      role <- maybe (raise $ "unknown player-role code: " ++ show rc) return $ decodeShipRole rc
      return (role, shipId, (px, py), (vx, vy), (x4, x5, x6, x7))
    _                                                                                             ->
      raise $ "unknown ship expression: " ++ show ship

  cmds <- maybe (raise $ "failed to convert commands to list: " ++ show x) return $ toList cmdse

  return ((gtick, x1), (role, shipId, pos, vel, other), cmds)

type GameState = (Expr, (ShipRole, Expr, (Int, Int), (Int, Int)), [Expr])

type Response  = (GameStage, ShipRole, GameState)

decodeResponse_ :: Expr -> Either String (ResponseTag, Maybe Response)
decodeResponse_ x = do
  let raise = Left . ("decodeResponse: " ++)
  ees     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x
  (c, es) <- case ees of
    []               ->  raise $ "error. response list is nil."
    Prim (Num c):es  ->  return (c, es)
    e:_              ->  raise $ "unknown response expression: " ++ show e
  t <- maybe (raise $ "unknown response code: " ++ show c) return $ decodeResponseCode c
  body <- case t of
            WRONG_REQUEST                            -> return Nothing
            GAME_STAGE    -> case es of
              Prim (Num stc) : static : state : _ -> do
                gst <- maybe (raise $ "unkown game-stage code: " ++ show stc) return $ decodeGameStage stc
                (_x0, role, _x2, _x3, _x4) <- decodeStaticInfo static
                ((gtick, _x1), (shipRole, shipId, pos, vel, _x4567), cmds) <- decodeGameState state
                return $ Just (gst, role, (gtick, (shipRole, shipId, pos, vel), cmds))
              _                                   ->
                raise $ "unknown gameStage response: " ++ show es

  return (t, body)

decodeResponse :: Expr -> Either String Response
decodeResponse x = do
  (t, body) <- decodeResponse_ x
  case t of
    WRONG_REQUEST -> Left "decodeResponse: error. request was wrong."
    GAME_STAGE    -> maybe (Left "decodeResponse: should not be happen") return body
