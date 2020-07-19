module Game (
  RequestTag (..),
  create,
  makeRequest,

  ResponseTag (..),
  ) where

import Message (Expr (Prim), Prim (Num), fromList)

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

{-
decodeResponseCode :: Int -> Maybe ResponseTag
decodeResponseCode =
    dispatch
  where
    dispatch 1 = Just WRONG_REQUEST
    dispatch 2 = Just GAME_STAGE
    dispatch _ = Nothing

-- decodeResponse e =
 -}
