{-# LANGUAGE NamedFieldPuns #-}
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
      Action A -> flipWorld Horizontal w
      Action D -> flipWorld Vertical w
      Action E -> rotate Clock w
      Action Q -> rotate Counter w
      Action R -> reset w
      Quit -> quitWorld w
      _ -> w

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Coord
dirToCoord d
  | d == Up = (0, -1)
  | d == Down = (0, 1)
  | d == Left = (-1, 0)
  | d == Right = (1, 0)
  | otherwise = (0, 0)

-- | handleDir @w@ world will change with @input@
handleDir :: Direction -> World -> World
handleDir input w = (w {wHero = newCoord})
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w)|+| dirToCoord input
    hConst i = max 0 (min i 640)
    newX = hConst heroX
    newY = hConst heroY

-- | flipWorld
flipWorld :: FlipDirection -> World -> World
flipWorld Horizontal w = w { flipped = (h', v') }
  where
    h' = not (fst(flipped w))
    v' = snd (flipped w)
flipWorld Vertical w = w { flipped = (h', v') }
  where
    h' = fst(flipped w)
    v' = not (snd (flipped w))

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> Int -> World
mkWorld (x, y) (width, height) sz = World
  { wHero = (x, y)
  , screenWidth = width
  , screenHeight = height
  , wHeroX = sz
  , wHeroY = sz
  , degrees = 0
  , flipped = (False, False)
  , exiting = False
  }

-- | reset
reset :: World -> World
reset w = w { degrees = 0, flipped = (False, False) }

-- | rotate
rotate :: RotateDirection -> World -> World
rotate Clock   w = w { degrees = degrees w + 15 }
rotate Counter w = w { degrees = degrees w - 15 }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
