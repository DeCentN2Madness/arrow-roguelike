{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

import Prelude hiding (Left, Right)

type Coord = (Int, Int)

data World = World
  { wHero :: Coord
  , screenWidth :: Int
  , screenHeight :: Int
  , wHeroX :: Int
  , wHeroY :: Int
  , degrees :: Int
  , flipped :: (Bool, Bool)
  , exiting :: Bool
  } deriving (Read, Show)

data Intent
  = Action Direction
  | Idle
  | Quit

data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  | A
  | D
  | E
  | Q
  | R
  deriving (Eq, Show)

data FlipDirection = Horizontal | Vertical

data RotateDirection = Clock | Counter
