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

-- | mkGrid (x,y) Coord for the world
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]
  where
    maxXY = if maxX > maxY then maxX else maxY

-- | mkWorld build the World
mkWorld :: StdGen -> Coord -> Coord -> Int -> Int -> World
mkWorld gen (x, y) (width, height) xMax yMax = let
  (d, g) = rogueDungeon xMax yMax gen
  in World { gameGen = gen
           , wHero = (x, y)
           , cameraXY = (0, 0)
           , degrees = 0
           , gridXY = (xMax, yMax)
           , grid = mkGrid xMax yMax
           , levelXY = (2.0 * fromIntegral width, 2.0 * fromIntegral height)
           , screenXY = (fromIntegral width, fromIntegral height)
           , scaleXY = (25.0, 25.0)
           , dungeon = d
           , exiting = False
           }

data Intent
  = Action Direction
  | Idle
  | Quit

data RotateDirection = Clock | Counter

data World = World
  { gameGen :: StdGen
  , wHero :: Coord
  , cameraXY :: (Double, Double)
  , degrees :: Int
  , gridXY :: Coord
  , grid :: [Coord]
  , levelXY :: (Double, Double)
  , screenXY :: (Double, Double)
  , scaleXY :: (Double, Double)
  , dungeon :: Dungeon
  , exiting :: Bool
  } deriving (Show)
