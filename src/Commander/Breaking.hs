module Commander.Breaking where

import Game
  (Commander, StageTag (Finished), GameState (..), ShipInfo (..),
   playerRole, Command (..))
import Vec (vneg, vsignum)

breaking :: Commander
breaking Finished _          _      =    return []
breaking _stag    staticInfo gstate = do
  let myRole = playerRole staticInfo
      --enemyRole = oppositeRole myRole

      ships = gstateShips gstate
      myShips    = [ ship | ship <- ships, shipRole ship == myRole ]
      -- enemyShips = [ ship | ship <- ships, shipRole ship == enemyRole ]

      -- Accelerate 命令の仕様で、加速度ベクトルは逆向きに与える.

      -- 中心が (0,0) であることを利用して、
      -- 自分の座標位置の signum の逆と 速度の signum の和を与え、
      -- 速度低下させつつ中心から遠ざかることを狙う
      {-
      commands =
        [ Accelerate (shipId ship) ( vneg (vsignum $ shipPos ship) <+> vsignum (shipVel ship) )
        | ship <- myShips ]
       -}

      -- 中心が (0,0) であることを利用して、
      -- 自分の座標位置の signum の逆を与え、
      -- 中心から遠ざかることを狙う
      commands_ =
        [ (Accelerate (shipId ship) (vneg $ vsignum $ shipPos ship), shipPos ship)
        | ship <- myShips ]

      commands = map fst commands_

      dprint (cmd, pos) = putStrLn $ "breaking: " ++ show cmd ++ " : " ++ show pos

  mapM_ dprint commands_
  return commands
