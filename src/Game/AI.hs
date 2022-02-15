{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (adjacent
               , aiAction
               , chessDist
               , distance
               , pathFinder) where

import Engine.Arrow.Data (World(..))
import Data.List
import qualified Game.Combat as GC
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

data AI
  = Attack
  | Move
  | Wait
  deriving (Show, Eq)

type Point = (Int, Int)

-- | aiAction
-- handle actions based on goal
-- TODO goal is affected by hitPoint, patrol, status; npc, so on...
aiAction :: [(Int, EntityKind)] -> World -> World
aiAction [] w = w
aiAction ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then aiAction xs w
  else let
  (_, pPos) = GP.getPlayer (entityT w)
  actionList = [ x | xy <- moveT mEntity,
                 let x = if adjacent xy pPos
                       then Attack
                       else Wait ]
  action = if not (null actionList)
    then head actionList
    else Wait
  newWorld = case action of
    Attack -> GC.mkCombat mx 0 w
    Move   -> w
    Wait   -> w
  in aiAction xs newWorld

-- | adjacent -- checks whether two points are adjacent
adjacent :: Point -> Point -> Bool
{-# INLINE adjacent #-}
adjacent s t = chessDist s t == 1

-- | chessDist - Chess distance between two points.
chessDist :: Point -> Point -> Int
chessDist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

-- | distance - Euclidean distance between two points.
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = let
  distX = fromIntegral $ (x2 - x1) ^ (2 :: Int)
  distY = fromIntegral $ (y2 - y1) ^ (2 :: Int)
  in sqrt (distX + distY)

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
  distanceList = [ (d, xy) | xy <- moveT mEntity,
                  let d = chessDist pPos xy ]
  moveList = coordF $ [ xy | (d, pos) <- sort distanceList,
                        let xy = if adjacent mPos pPos || d > 5
                              then mPos
                              else pos ]
  move = if null moveList
    then mPos
    else head moveList
  -- move w/ blockT
  newWorld = w { entityT = GE.updateEntityPos mx move (entityT w) }
  in pathFinder xs newWorld
