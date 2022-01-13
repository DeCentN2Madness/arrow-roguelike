{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module ArrowData where

import Prelude hiding (Left, Right)
import qualified SDL

data AssetMap a = AssetMap
  { background :: a
  , hero :: a
  , wall :: a
  , stairDown :: a
  , stairUp :: a
  } deriving (Functor, Foldable, Traversable)

assetPaths :: PathMap
assetPaths = AssetMap
  { background = "./assets/Background.png"
  , hero = "./assets/Hero.png"
  , wall = "./assets/Wall.png"
  , stairDown = "./assets/StairDown.png"
  , stairUp = "./assets/StairUp.png"
  }

type Coord = (Int, Int)

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
  | S
  | W
  deriving (Eq)

data Intent
  = Action Direction
  | Idle
  | Quit

type PathMap = AssetMap FilePath

data RotateDirection = Clock | Counter

type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)

data World = World
  { wHero :: Coord
  , screenWidth :: Int
  , screenHeight :: Int
  , wSize :: Int
  , degrees :: Int
  , exiting :: Bool
  } deriving (Read, Show)
