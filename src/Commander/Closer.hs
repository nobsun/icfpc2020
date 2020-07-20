module Commander.Closer where

import Vec ((<+>), (<->), vsignum, vquot)
import Game
  (Commander, StageTag (Finished), GameState (..), ShipInfo (..),
   playerRole, oppositeRole, Command (..))

closer :: Commander
closer Finished _          _      =    return []
closer _stag    staticInfo gstate = do
  let myRole = playerRole staticInfo
      enemyRole = oppositeRole myRole

      ships = gstateShips gstate
      myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
      enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

      -- Accelerate 命令の仕様で、加速度ベクトルは逆向きに与える.
      -- 敵の集団に近づく加速度
      closerAcc pos vel =
        foldr (<+>) (0,0)
        [ vsignum (npos <-> (shipPos enemy <+> shipVel enemy))
        | enemy <- enemyShips
        , let npos = pos <+> vel
              _npos = pos <+> vel `vquot` 2]

      commands =
        [ Accelerate (shipId ship) (closerAcc (shipPos ship) (shipVel ship))
        | ship <- myShips ]

  return commands
