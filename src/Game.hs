module Game (
  RequestTag (..),
  create,
  makeRequest,

  ResponseTag (..),
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

decodeResponse_ :: Expr -> Either String (ResponseTag, Maybe (Int, Expr, Expr))
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
              Prim (Num gst) : staticKey : state : _ -> return $ Just (gst, staticKey, state)
              _                                      -> raise $ "unknown gameStage response: " ++ show es

  return (t, body)

decodeResponse :: Expr -> Either String (Int, Expr, Expr)
decodeResponse x = do
  (t, body) <- decodeResponse_ x
  case t of
    WRONG_REQUEST -> Left "decodeResponse: error. request was wrong."
    GAME_STAGE    -> maybe (Left "decodeResponse: should not be happen") return body
