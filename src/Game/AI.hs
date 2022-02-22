{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (aiAction
               , pathFinder) where

import Data.List
import Engine.Arrow.Data (World(..))
import qualified Engine.Arrow.Compass as EAC
import qualified Game.Combat as GC
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

data AI
  = Attack
  | Wait
  deriving (Show, Eq)

-- | aiAction
-- handle actions based on goal
-- TODO goal is affected by hitPoint, patrol, status; npc, so on...
aiAction :: [(Int, EntityKind)] -> World -> World
aiAction [] w = w
aiAction ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then aiAction xs w
  else let
  (_, pPos) = GP.getPlayer (entityT w)
  mPos      = coord mEntity
  action    = if EAC.adjacent pPos mPos
    then Attack
    else Wait
  newWorld = case action of
    Attack -> GC.mkCombat mx 0 w
    Wait   -> w
  -- newWorld w/ action
  in aiAction xs newWorld

-- | pathFinder
-- 0. Don't move the Player at 0
-- 1. Distance from goal
-- 2. Decide by distance
-- 3. Check blockList
-- 4. Update move
pathFinder :: [(Int, EntityKind)] -> World -> World
pathFinder [] w = w
pathFinder ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then pathFinder xs w
  else let
  coordF :: [(Int, Int)] -> [(Int, Int)]
  coordF = filter (`notElem` blockT)
  blockT = [ xy | (_, xy) <- GE.fromBlock (entityT w) ]
  (_, pPos) = GP.getPlayer (entityT w)
  mPos      = coord mEntity
  distList  = [ (d, xy) | xy <- moveT mEntity,
                let d = EAC.chessDist pPos xy ]
  moveList  = coordF $ [ xy | (d, pos) <- sort distList,
                        let xy = if EAC.adjacent mPos pPos || d > 5
                              then mPos
                              else pos ]
  move = if null moveList
    then mPos
    else head moveList
  in pathFinder xs w { entityT = GE.updateEntityPos mx move (entityT w) }
