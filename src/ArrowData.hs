{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

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
  , gridX :: Int
  , gridY :: Int
  , screenWidth :: Int
  , screenHeight :: Int
  , xScale :: Double
  , yScale :: Double
  , degrees :: Int
  , exiting :: Bool
  } deriving (Read, Show)
