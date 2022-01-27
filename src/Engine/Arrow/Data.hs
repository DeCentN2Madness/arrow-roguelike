{-

ArrowData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Data where

import Control.Monad.Random (StdGen)
import Dungeon (Dungeon, rogueDungeon)
import GameData (EntityMap, GameMap, mkEntityMap, mkGameMap)

type Coord = (Int, Int)

data Direction
  = Help
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  | K
  | Y
  | H
  | B
  | J
  | U
  | L
  | N
  | R
  deriving (Eq)

data Intent
  = Action Direction
  | Idle
  | Quit

data World = World
  { -- the Dungeon
  gameGen :: StdGen
  , dungeon :: Dungeon
  -- Coord for Hero
  , gameT :: GameMap
  , entityT :: EntityMap
  , fovT :: [Coord]
  -- XY for Screen
  , gridXY :: Coord
  , cameraXY :: (Double, Double)
  , screenXY :: (Double, Double)
  , scaleXY :: (Double, Double)
  -- GameStates
  , journal :: String
  , dirty   :: Bool
  , starting :: Bool
  , exiting :: Bool
  } deriving (Show)

-- | mkWorld build the World
mkWorld :: StdGen -> Coord -> Int -> Int -> World
mkWorld gen (width, height) xMax yMax = let
  (d, g) = rogueDungeon xMax yMax gen
  sx = 25.0 -- scaleXY based on tiles
  sy = 25.0
  in World { gameGen = g
           , dungeon = d
           , gameT = mkGameMap d
           , entityT = mkEntityMap
           , fovT = []
           , gridXY = (xMax, yMax)
           , cameraXY = (0.0, 0.0)
           , screenXY = (fromIntegral width, fromIntegral height)
           , scaleXY = (sx, sy)
           , journal = []
           , dirty = True
           , starting = True
           , exiting = False
           }
