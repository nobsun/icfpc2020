module Commander.Shooter where

import Message (num, fromList)
import Vec ((<+>))
import Game
  (Commander, StageTag (Finished), GameState (..), ShipInfo (..),
   playerRole, oppositeRole, Command (..))

shooter :: Commander
shooter Finished _          _      =    return []
shooter _stag    staticInfo gstate = do
  let myRole = playerRole staticInfo
      enemyRole = oppositeRole myRole

      ships = gstateShips gstate
      myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
      enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

      firstTarget = take 1 enemyShips

      commands =
        [ Shoot (shipId ship) (shipPos enemy <+> shipVel enemy) $ fromList [num n | n <- [30, 57, 4]]
        | ship <- myShips
        , enemy <- firstTarget ]

  return commands
