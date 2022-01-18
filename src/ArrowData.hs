{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

import Dungeon (Dungeon)

type Coord = (Int, Int)

data Direction
  = Help
  | North
  | South
  | East
  | West
  | A
  | D
  | E
  | Q
  | R
  | S
  | W
  deriving (Eq)

data Intent
  = Action Direction
  | Idle
  | Quit

data RotateDirection = Clock | Counter

data World = World
  { wHero :: Coord
  , cameraXY :: (Double, Double)
  , degrees :: Int
  , gridXY :: Coord
  , grid :: [Coord]
  , levelXY :: (Double, Double)
  , screenXY :: (Double, Double)
  , scaleXY :: (Double, Double)
  , dungeon :: Dungeon
  , exiting :: Bool
  } deriving (Read, Show)
