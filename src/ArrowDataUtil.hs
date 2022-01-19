{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil(applyIntent) where

import qualified Data.Vector as V
import ArrowData
import Dungeon (dungeonTiles, getTerrainAt, rogueDungeon, Terrain(..))
import Camera (updateCamera)

-- | applyIntent
applyIntent :: Intent -> World -> World
applyIntent intent w = w'
  where
    w' = case intent of
      Action North -> handleDir North w
      Action South -> handleDir South w
      Action East -> handleDir East w
      Action West -> handleDir West w
      Action A -> handleDir East w
      Action D -> handleDir West w
      Action E -> rotate Clock w
      Action Q -> rotate Counter w
      Action R -> reset w
      Action S -> handleDir South w
      Action W -> handleDir North w
      Quit -> quitWorld w
      _ -> w

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Coord
dirToCoord d
  | d == North = (0, -1)
  | d == South = (0, 1)
  | d == East = (-1, 0)
  | d == West = (1, 0)
  | otherwise = (0, 0)

-- | handleDir @w@ world will change with @input@
-- horiz, vert check the entire grid
-- getTerrainAt checks the dungeon
handleDir :: Direction -> World -> World
handleDir input w = if (starting w)
    then do
      let terrainList = V.toList $ dungeonTiles $ dungeon w
          openList = filter((== Open).fst) $ zip terrainList (grid w)
          startPos = snd $ head openList
      updateCamera w { wHero = startPos, starting = False }
    else
      updateCamera newWorld
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w) |+| dirToCoord input
    horiz i = max 0 (min i (fst $ gridXY w))
    vert  j = max 0 (min j (snd $ gridXY w))
    newX = horiz heroX
    newY = vert heroY
    newWorld = case getTerrainAt (newX, newY) (dungeon w) of
      Wall -> w
      Rubble -> w
      _ -> w { wHero = newCoord }


-- | reset
-- reset the world and redraw the dungeon
reset :: World -> World
reset w = let
  (d, g) = rogueDungeon (fst $ gridXY w) (snd $ gridXY w) (gameGen w)
  in w { gameGen = g, dungeon = d, degrees = 0, starting = True }

-- | rotate
rotate :: RotateDirection -> World -> World
rotate Clock   w = w { degrees = degrees w + 15 }
rotate Counter w = w { degrees = degrees w - 15 }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
