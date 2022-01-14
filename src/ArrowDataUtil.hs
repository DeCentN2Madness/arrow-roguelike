{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil(applyIntent, mkDungeon, mkWorld) where

import qualified Data.Vector as V
import ArrowData

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

-- | getTerrainAt
getTerrainAt :: (Int, Int) -> Dungeon -> Terrain
getTerrainAt (x, y) d = let
  width = dungeonWidth d
  height = dungeonHeight d
  index = y * width + x
  in if y < 0 || x < 0 || y >= height || x >= width
    then Wall
    else V.unsafeIndex (dungeonTiles d) index

-- | handleDir @w@ world will change with @input@
-- horiz, vert check the entire grid
-- getTerrainAt checks the dungeon
handleDir :: Direction -> World -> World
--handleDir input w = (w {wHero = newCoord})
handleDir input w = newWorld
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w)|+| dirToCoord input
    horiz i = max 0 (min i (gridX w))
    vert j = max 0 (min j (gridY w))
    newX = horiz heroX
    newY = vert heroY
    newWorld = case getTerrainAt (newX, newY) (dungeon w) of
      Wall -> w
      _ -> w { wHero = newCoord}

-- | mkDungeon build the dungeon
mkDungeon :: Int -> Int -> Dungeon
mkDungeon xMax yMax = Dungeon xMax yMax $
  V.generate (xMax*yMax) (\i -> let (y, x) = i `divMod` xMax
    in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
      then Wall
      else Open)

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> Int -> Int -> World
mkWorld (x, y) (width, height) xMax yMax = World
  { wHero = (x, y)
  , gridX = xMax
  , gridY = yMax
  , screenWidth = width
  , screenHeight = height
  , xScale = 39.0 :: Double
  , yScale = 39.0 :: Double
  , degrees = 0
  , exiting = False
  , dungeon = mkDungeon xMax yMax
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
