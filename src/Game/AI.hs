{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (aiAction, pathFinder) where

import Data.List
import qualified Data.Map as Map
import Engine.Arrow.Data (World(..))
import qualified Engine.Arrow.Compass as EAC
import qualified Game.Combat as GC
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

data AI
  = Attack
  | Heal
  | Wait
  deriving (Show, Eq)

-- | aiAction
-- AI actions based on goal, HP, position...
-- TODO more actions, like throw, use items...
aiAction :: [(Int, EntityKind)] -> World -> World
aiAction [] w = w
aiAction ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then aiAction xs w
  else let
  (_, pPos) = GP.getPlayer (entityT w)
  -- monster properties
  mPos  = coord mEntity
  mProp = property mEntity
  spawn = read $ Map.findWithDefault (show mPos) "spawn" mProp :: (Int, Int)
  -- action
  action
    | EAC.adjacent pPos  mPos = Attack
    | EAC.adjacent spawn mPos = Heal
    | otherwise = Wait
  world = case action of
    Attack -> GC.mkCombat mx 0 w
    Heal -> monsterHeal (mx, mEntity) w
    Wait -> w
  -- newWorld w/ action
  in aiAction xs world

-- | monsterHeal
-- M heals slowly...
monsterHeal :: (Int, EntityKind) -> World -> World
monsterHeal (mx, mEntity) w = let
  heal = eHP mEntity + 1
  hp = if heal > eMaxHP mEntity then eMaxHP mEntity else heal
  in w { entityT = GE.updateEntityHp mx hp (entityT w) }

-- | pathFinder
-- 0. Don't move the Player at 0
-- 1. Distance from goal
--   a. 5 or less is *critical*
-- 2. Decide by distance
--    a. 4 is Vision
--    b. 7 is Hear
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
  -- monster properties
  mPos  = coord mEntity
  mProp = property mEntity
  spawn = read $ Map.findWithDefault (show pPos) "spawn" mProp :: (Int, Int)
  -- flee goal if *critical* eHP
  goal = if eHP mEntity <= 5 then spawn else pPos
  -- distance based on goal
  distList = [ (d, xy) | xy <- moveT mEntity, let d = EAC.chessDist goal xy ]
  moveList = coordF $
    [ xy | (d, pos) <- sort distList,
      let xy = if EAC.adjacent mPos goal || d >= 7 then mPos else pos ]
  move = if null moveList then mPos else head moveList
  in pathFinder xs w { entityT = GE.updateEntityPos mx move (entityT w) }
