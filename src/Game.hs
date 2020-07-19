module Game where

import Message (Expr (Prim), Prim (Num), fromList)

data PacketTag
  = GAME_STAGE
  | JOIN
  | START
  | COMMANDS
  deriving (Eq, Show)

requestCode :: PacketTag -> Int
requestCode =
    code
  where
    code GAME_STAGE = 1
    code JOIN       = 2
    code START      = 3
    code COMMANDS   = 4

makeRequest :: Int -> PacketTag -> Expr -> Expr
makeRequest playerKey rtag dataExpr =
  fromList [ Prim $ Num $ requestCode rtag, Prim $ Num playerKey, dataExpr ]
