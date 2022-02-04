{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (pathFinder) where

import Data.List
import Game.Kind.Entity (Entity(..), EntityKind(..))

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) = let
  distX = fromIntegral $ (x1 - x2) * (x1 - x2)
  distY = fromIntegral $ (y1 - y2) * (y1 - y2)
  in sqrt (distX + distY)

-- | pathFinder
-- 1. distance from goal
-- 2. decide by distance
-- 3. stop at distance 1
-- 3. update pos
pathFinder :: (Int, Int) -> [(Int, Int)] -> EntityKind -> (Int, Int)
pathFinder goal move e = if kind e == Actor
  then coord e
  else let
  distanceList = [ (d, v) | v <- move,
                   let d = distance goal v ]
  -- filter based on +1 FoV
  -- stop at distance 1
  moveList = [ xy | (d, v) <- sort distanceList,
               let xy = if d == 1 || d > 5 then coord e else v ]
  in head moveList
