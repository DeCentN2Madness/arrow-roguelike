{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(mkGameData) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Dungeon (Dungeon(..), Terrain(..))

type Coord = (Int, Int)

type GameMap = Map.Map Coord GameData

data GameData = GameData
  { visible :: Bool
  , kind :: Terrain
  } deriving (Show)

listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  tile = GameData { visible = False, kind = k }
  in Map.insert v tile (listToMap xs)

mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

mkGameData :: Dungeon -> GameMap
mkGameData d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  terrainList = V.toList $ dungeonTiles d
  tileList = zip terrainList grid
  in listToMap tileList
