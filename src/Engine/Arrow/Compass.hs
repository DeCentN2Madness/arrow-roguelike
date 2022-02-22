{-

Engine.Arrow.Compass.hs

Compass Coord for the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Compass ((|+|)
                            , (|-|)
                            , abovePoint
                            , adjacent
                            , aboveOrCollinearPoint
                            , belowOrCollinearPoint
                            , belowPoint
                            , cardinal
                            , chessDist
                            , clamp
                            , collinearPoint
                            , collinearLine
                            , dirToCoord
                            , distance) where

import Engine.Arrow.Data (Coord, Direction(..))

type Point = (Int,Int)
data SightLine = SightLine Int Int Int Int deriving (Eq, Show)

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | operator to sub 2 coordinates together
(|-|) :: Coord -> Coord -> Coord
(|-|) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | abovePoint infix
abovePoint :: SightLine -> Point -> Bool
{-# INLINE abovePoint #-}
abovePoint self p = relativeSlope self p < 0

-- | aboveOrCollinearPoint infix
aboveOrCollinearPoint :: SightLine -> Point -> Bool
{-# INLINE aboveOrCollinearPoint #-}
aboveOrCollinearPoint self p = relativeSlope self p <= 0

-- | adjacent -- checks whether two points are adjacent
adjacent :: Point -> Point -> Bool
{-# INLINE adjacent #-}
adjacent s t = chessDist s t == 1

-- | belowOrCollinearPoint infix
belowOrCollinearPoint :: SightLine -> Point -> Bool
{-# INLINE belowOrCollinearPoint #-}
belowOrCollinearPoint self p = relativeSlope self p >= 0

-- | belowPoint infix
belowPoint :: SightLine -> Point -> Bool
{-# INLINE belowPoint #-}
belowPoint self p = relativeSlope self p > 0

-- | chessDist - Chess distance between two points.
chessDist :: Point -> Point -> Int
{-# INLINE chessDist #-}
chessDist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

-- | clamp to grid
cardinal :: Coord -> [Coord]
cardinal pos = [ pos |+| dirToCoord North
               , pos |+| dirToCoord NorthEast
               , pos |+| dirToCoord East
               , pos |+| dirToCoord SouthEast
               , pos |+| dirToCoord South
               , pos |+| dirToCoord SouthWest
               , pos |+| dirToCoord West
               , pos |+| dirToCoord NorthWest ]

-- | clamp to grid
clamp :: Coord -> Coord -> Coord
clamp (x1, y1) (x2, y2) = let
  horiz i = max 0 (min i x2)
  vert  j = max 0 (min j y2)
  newX = horiz x1
  newY = vert y1
  in (newX, newY)

-- | collinearPpoint infix
collinearPoint :: SightLine -> Point -> Bool
{-# INLINE collinearPoint #-}
collinearPoint self p = relativeSlope self p == 0

-- | collinearLine infix
collinearLine :: SightLine -> SightLine -> Bool
{-# INLINE collinearLine #-}
collinearLine self (SightLine xi yi xf yf) =
  collinearPoint self (xi, yi) && collinearPoint self (xf, yf)

-- | distance - Euclidean distance between two points.
distance :: Point -> Point -> Double
{-# INLINE distance #-}
distance (x1, y1) (x2, y2) = let
  distX = fromIntegral $ (x2 - x1) ^ (2 :: Int)
  distY = fromIntegral $ (y2 - y1) ^ (2 :: Int)
  in sqrt (distX + distY)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Coord
dirToCoord d
  | d == North = (0, -1)
  | d == NorthEast = dirToCoord North |+| dirToCoord East
  | d == East  = (-1, 0)
  | d == SouthEast = dirToCoord South |+| dirToCoord East
  | d == South = (0, 1)
  | d == SouthWest = dirToCoord South |+| dirToCoord West
  | d == West  = (1, 0)
  | d == NorthWest = dirToCoord North |+| dirToCoord West
  | otherwise  = (0, 0)

-- | relativeSlope infix
relativeSlope :: SightLine -> Point -> Int
relativeSlope (SightLine xi yi xf yf) (x, y) = let
  dx = xf - xi
  dy = yf - yi
  in (dy * (xf - x)) - (dx * (yf - y))
