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
  } deriving (Show)

data Intent
  = SelectSurface Direction
  | Idle
  | Quit

data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  | H
  | X
  deriving (Eq)

data SurfaceMap a = SurfaceMap
  { help :: a
    , up :: a
    , down :: a
    , left :: a
    , right :: a
    , _h :: a
    , _x :: a
  } deriving (Foldable, Traversable, Functor)

surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap
  { help = "./assets/press.bmp"
    , up = "./assets/up.bmp"
    , down = "./assets/down.bmp"
    , left = "./assets/left.bmp"
    , right = "./assets/right.bmp"
    , _h = "./assets/hero.bmp"
    , _x = "./assets/x.bmp"
  }
