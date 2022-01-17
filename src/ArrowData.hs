{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

import Data.Vector (Vector)

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

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
  } deriving (Read, Show)

data Intent
  = Action Direction
  | Idle
  | Quit

data RotateDirection = Clock | Counter

data Terrain
  = Open
  | Wall
  | StairsDown
  | StarsUp
  deriving (Read, Show, Eq)

data World = World
  { wHero :: Coord
  , gridXY :: Coord
  , screenXY :: (Double, Double)
  , levelXY :: (Double, Double)
  , cameraXY :: (Double, Double)
  , scaleXY :: (Double, Double)
  , degrees :: Int
  , exiting :: Bool
  , dungeon :: Dungeon
  , grid :: [Coord]
  } deriving (Read, Show)
