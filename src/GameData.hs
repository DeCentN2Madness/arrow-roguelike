{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(GameData
               , GameMap
               , fromGameMap
               , mkGameMap) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Dungeon (Dungeon(..), Terrain(..))

type Coord = (Int, Int)

type GameMap = Map.Map Coord GameData

data GameData = GameData
  { visible :: Bool
  , kind :: Terrain
  } deriving (Show)

terrainMap :: [Coord] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  kinds = case Map.lookup x gm of
    Just k -> (kind k)
    _      -> Zero
  in [(kinds, x)] ++ terrainMap xs gm

fromGameMap :: GameMap -> Terrain -> [Coord]
fromGameMap gm t = let
  xy = Map.keys gm
  terrainList = filter ((==t).fst) $ terrainMap xy gm
  coordList = [v | (_, v) <- terrainList]
  in coordList

listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  tile = GameData { visible = False, kind = k }
  in Map.insert v tile (listToMap xs)

mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

mkGameMap :: Dungeon -> GameMap
mkGameMap d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  terrainList = V.toList $ dungeonTiles d
  tileList = zip terrainList grid
  in listToMap tileList
