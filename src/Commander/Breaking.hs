module Commander.Breaking where

import Game
  (Commander, StageTag (Finished), GameState (..), ShipInfo (..),
   playerRole, Command (..))

breaking :: Commander
breaking Finished _          _      =    return []
breaking _stag    staticInfo gstate = do
  let myRole = playerRole staticInfo
      --enemyRole = oppositeRole myRole

      ships = gstateShips gstate
      myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
      -- enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

      -- Accelerate 命令の仕様で、加速度ベクトルは逆向きに与える.
      -- 速度と同じベクトルを与えて速度を打ち消すことを狙う
      commands =
        [ Accelerate (shipId ship) (shipVel ship)
        | ship <- myShips ]

  return commands
