{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module ArrowData where

import Control.Monad.Random (StdGen)
import Dungeon (Dungeon, rogueDungeon)

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
  { grid :: [Coord]
  , dungeon :: Dungeon
  -- Coord for Hero
  , fovT :: [Coord]
  , gameGen :: StdGen
  , wHero :: Coord
  , degrees :: Int
  , gridXY :: Coord
  -- XY for Screen
  , cameraXY :: (Double, Double)
  , levelXY :: (Double, Double)
  , screenXY :: (Double, Double)
  , scaleXY :: (Double, Double)
  -- GameStates
  , dirty   :: Bool
  , starting :: Bool
  , exiting :: Bool
  } deriving (Show)

-- | mkGrid (x,y) uniform Coord for the World
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

-- | mkWorld build the World
mkWorld :: StdGen -> Coord -> Int -> Int -> World
mkWorld gen (width, height) xMax yMax = let
  (d, g) = rogueDungeon xMax yMax gen
  sx = 25.0 -- scaleXY based on tiles
  sy = 25.0
  in World { grid = mkGrid xMax yMax
           , dungeon = d
           , fovT = [(0,0)]
           , gameGen = g
           , wHero = (0, 0)
           , degrees = 0
           , gridXY = (xMax, yMax)
           , cameraXY = (0.0, 0.0)
           , levelXY = (sx * fromIntegral xMax, sy * fromIntegral yMax)
           , screenXY = (fromIntegral width, fromIntegral height)
           , scaleXY = (sx, sy)
           , dirty = True
           , starting = True
           , exiting = False
           }
