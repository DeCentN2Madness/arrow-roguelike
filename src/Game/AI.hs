{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (pathFinder) where

import Engine.Arrow.Data (World(..))
import Data.List
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) = let
  distX = fromIntegral $ (x1 - x2) * (x1 - x2)
  distY = fromIntegral $ (y1 - y2) * (y1 - y2)
  in sqrt (distX + distY)

-- | pathFinder
-- 0. Don't move the Player at 0
-- 1. Distance from goal
-- 2. Decide by distance
-- 3. Check blockList
-- 4. Update coord
-- TODO goal is affected by hitPoint, patrol, status; npc, so on...
pathFinder :: [(Int, EntityKind)] -> World -> World
pathFinder [] w = w
pathFinder ((mx, mEntity):xs) w = if mx == 0
  then pathFinder xs w
  else let
  coordF :: [(Int, Int)] -> [(Int, Int)]
  coordF = filter (`notElem` blockT)
  blockT = [ xy | (_, xy) <- GE.fromBlock (entityT w) ]
  (_, pPos) = GE.getPlayer (entityT w)
  distanceList = [ (d, xy) | xy <- moveT mEntity,
                  let d = distance pPos xy ]
  moveList = coordF $ [ xy | (d, pos) <- sort distanceList,
                        let xy = if d==1 || d > 5 then coord mEntity else pos ]
  move = if not (null moveList)
    then head moveList
    else coord mEntity
  -- move w/ blockT
  newWorld = w { entityT = GE.updateEntityPos mx move (entityT w) }
  in pathFinder xs newWorld
