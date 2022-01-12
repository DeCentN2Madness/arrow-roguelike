{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

import Prelude hiding (Left, Right)

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

data SurfaceMap a = SurfaceMap
  { help :: a
    , up :: a
    , down :: a
    , left :: a
    , right :: a
    , h :: a
    , x :: a
  } deriving (Foldable, Traversable, Functor)

surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap
  { help = "./assets/press.bmp"
    , up = "./assets/up.bmp"
    , down = "./assets/down.bmp"
    , left = "./assets/left.bmp"
    , right = "./assets/right.bmp"
    , h = "./assets/hero.bmp"
    , x = "./assets/x.bmp"
  }
