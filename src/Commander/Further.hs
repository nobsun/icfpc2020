module Commander.Further where

import Vec ((<+>), (<->), vsignum, vquot)
import Game
  (Commander, StageTag (Finished), GameState (..), ShipInfo (..),
   playerRole, oppositeRole, Command (..))

further :: Commander
further Finished _          _      =    return []
further _stag    staticInfo gstate = do
  let myRole = playerRole staticInfo
      enemyRole = oppositeRole myRole

      ships = gstateShips gstate
      myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
      enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

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

  return commands
