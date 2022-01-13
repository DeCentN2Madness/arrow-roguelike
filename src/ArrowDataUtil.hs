{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil(applyIntent, mkWorld) where

import Prelude hiding (Left, Right)
import ArrowData

-- | applyIntent
applyIntent :: Intent -> World -> World
applyIntent intent w = w'
  where
    w' = case intent of
      Action Up -> handleDir Up w
      Action Down -> handleDir Down w
      Action Left -> handleDir Left w
      Action Right -> handleDir Right w
      Action A -> handleDir Left w
      Action D -> handleDir Right w
      Action E -> rotate Clock w
      Action Q -> rotate Counter w
      Action R -> reset w
      Action S -> handleDir Down w
      Action W -> handleDir Up w
      Quit -> quitWorld w
      _ -> w

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Int -> Coord
dirToCoord d sz
  | d == Up = (0, -1 * sz)
  | d == Down = (0, 1 * sz)
  | d == Left = (-1 * sz, 0)
  | d == Right = (1 * sz, 0)
  | otherwise = (0, 0)

-- | handleDir @w@ world will change with @input@
handleDir :: Direction -> World -> World
handleDir input w = (w {wHero = newCoord})
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w)|+| dirToCoord input (wSize w)
    horiz i = max 0 (min i w')
    vert j = max 0 (min j h')
    newX = horiz heroX
    newY = vert heroY
    w' = (screenWidth w) - (wSize w)
    h' = (screenHeight w) - (wSize w)

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> Int -> World
mkWorld (x, y) (width, height) sz = World
  { wHero = (x, y)
  , screenWidth = width
  , screenHeight = height
  , wSize = sz
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
