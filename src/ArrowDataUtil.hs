{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil(applyIntent, mkWorld) where

import ArrowData
import Dungeon (Terrain(..))
import qualified Dungeon as DUN
import qualified Camera as CAM

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
handleDir input w = CAM.updateCamera newWorld
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w) |+| dirToCoord input
    horiz i = max 0 (min i (fst $ gridXY w))
    vert j = max 0 (min j (snd $ gridXY w))
    newX = horiz heroX
    newY = vert heroY
    newWorld = case DUN.getTerrainAt (newX, newY) (dungeon w) of
      Wall -> w
      Rubble -> w
      _ -> w { wHero = newCoord}

-- | mkGrid (x,y) Coord for the world
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]
  where
    maxXY = if maxX > maxY then maxX else maxY

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> Int -> Int -> World
mkWorld (x, y) (width, height) xMax yMax = World
  { wHero = (x, y)
  , cameraXY = (0, 0)
  , degrees = 0
  , gridXY = (xMax, yMax)
  , grid = mkGrid xMax yMax
  , levelXY = (2.0 * fromIntegral width, 2.0 * fromIntegral height)
  , screenXY = (fromIntegral width, fromIntegral height)
  , scaleXY = (25.0, 25.0)
  , dungeon = DUN.mkDungeon xMax yMax
  , exiting = False
  }

-- | reset
reset :: World -> World
reset w = w { degrees = 0 }

-- | rotate
rotate :: RotateDirection -> World -> World
rotate Clock   w = w { degrees = degrees w + 15 }
rotate Counter w = w { degrees = degrees w - 15 }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
