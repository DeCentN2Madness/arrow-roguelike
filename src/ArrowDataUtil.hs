{-

ArrowDataUtil.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowDataUtil where

import Prelude hiding (Left, Right)
import ArrowData


-- operator to add 2 coordinates together
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
handleDir :: World -> Direction -> World
handleDir w@(World hero) input = (w { wHero = newCoord})
  where
    newCoord = (newX, newY)
    (heroX, heroY) = hero |+| dirToCoord input
    hConst i = max 0 (min i 80)
    newX = hConst heroX
    newY = hConst heroY
