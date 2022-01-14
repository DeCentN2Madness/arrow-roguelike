{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil(applyIntent, mkWorld) where

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

-- | handleDir @w@ world will change with @input@
handleDir :: Direction -> World -> World
handleDir input w = (w {wHero = newCoord})
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w)|+| dirToCoord input
    horiz i = max 0 (min i (gridX w))
    vert j = max 0 (min j (gridY w))
    newX = horiz heroX
    newY = vert heroY

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> World
mkWorld (x, y) (width, height) = World
  { wHero = (x, y)
  , gridX = 80
  , gridY = 50
  , screenWidth = width
  , screenHeight = height
  , xScale = 40.0 :: Double
  , yScale = 40.0 :: Double
  , degrees = 0
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
