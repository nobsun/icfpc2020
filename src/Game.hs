module Game (
  RequestTag (..),
  create,
  makeRequest,

  ResponseTag (..),
  GameStage (..),
  PlayerRole (..),
  Response,
  decodeResponse,
  decodeResponse_,
  ) where

import Message (Expr (Prim), Prim (Num), fromList, toList)

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


data ResponseTag
  = WRONG_REQUEST
  | GAME_STAGE
  deriving (Eq, Show)

decodeResponseCode :: Int -> Maybe ResponseTag
decodeResponseCode =
    dispatch
  where
    dispatch 1 = Just WRONG_REQUEST
    dispatch 2 = Just GAME_STAGE
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

data PlayerRole
  = Attacker
  | Defender
  deriving (Eq, Show)

decodePlayerRole :: Int -> Maybe PlayerRole
decodePlayerRole =
    dispatch
  where
    dispatch 0 = Just Attacker
    dispatch 1 = Just Defender
    dispatch _ = Nothing

decodeStaticInfo :: Expr -> Either String (Expr, PlayerRole, Expr, Expr, Expr)
decodeStaticInfo x = do
  let raise = Left . ("decodeStaticInfo: " ++)
  es     <- maybe (raise $ "failed to convert to list: " ++ show x) return $ toList x
  case es of
    x0 : Prim (Num rc) : x2 : x3 : x4 : _ -> do
      role <- maybe (raise $ "unknown player-role code: " ++ show rc) return $ decodePlayerRole rc
      return (x0, role, x2, x3, x4)
    _                                     ->
      raise $ "unknown static-info expression: " ++ show es

type Response  = (GameStage, PlayerRole, Expr)

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
                (_, role, _, _, _) <- decodeStaticInfo static
                return $ Just (gst, role, state)
              _                                   ->
                raise $ "unknown gameStage response: " ++ show es

  return (t, body)

decodeResponse :: Expr -> Either String Response
decodeResponse x = do
  (t, body) <- decodeResponse_ x
  case t of
    WRONG_REQUEST -> Left "decodeResponse: error. request was wrong."
    GAME_STAGE    -> maybe (Left "decodeResponse: should not be happen") return body
