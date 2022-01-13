{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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
  } deriving (Read, Show)

data Intent
  = Action Direction
  | Idle
  | Quit
  deriving (Show)

data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  | H
  | X
  | W
  deriving (Eq, Show)

data ActionMap a = ActionMap
  { help :: a
  , _h :: a
  , _x :: a
  }
  deriving (Foldable, Traversable, Functor)

actionPaths :: ActionMap FilePath
actionPaths = ActionMap
  { help = "./assets/press.bmp"
  , _h = "./assets/hero.bmp"
  , _x = "./assets/x.bmp"
  }
