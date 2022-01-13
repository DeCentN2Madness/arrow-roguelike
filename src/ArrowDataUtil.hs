{-# LANGUAGE NamedFieldPuns #-}
{-

ArrowDataUtil.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil where

import Prelude hiding (Left, Right)
import ArrowData


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

-- | mkWorld build the World
mkWorld :: Coord -> Coord -> Int -> World
mkWorld (x, y) (width, height) sz = World
  { wHero = (x, y)
  , screenWidth = width
  , screenHeight = height
  , wHeroX = sz
  , wHeroY = sz
  }

-- | handleDir @w@ world will change with @input@
handleDir :: World -> Direction -> World
handleDir w input = (w {wHero = newCoord})
  where
    newCoord = (newX, newY)
    (heroX, heroY) = (wHero w)|+| dirToCoord input
    hConst i = max 0 (min i 640)
    newX = hConst heroX
    newY = hConst heroY

-- | handleEvent
handleEvent :: Intent -> World -> World
handleEvent intent w = n
  where
    delta = case intent of
      Action Up -> handleDir w Up
      Action Down -> handleDir w Down
      Action Left -> handleDir w Left
      Action Right -> handleDir w Right
      _ -> w
    n = delta
