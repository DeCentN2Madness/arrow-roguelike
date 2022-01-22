{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(GameData
               , GameMap
               , fromGameMap
               , fromHard
               , fromOpen
               , mkGameMap
               , updateGameMap) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Dungeon (Dungeon(..), Terrain(..))

type Coord = (Int, Int)

type GameMap = Map.Map Coord GameData

data GameData = GameData
  { visible :: Bool
  , kind :: Terrain
  } deriving (Show)

-- | fromGameMap list of Coord by Terrain @t@
fromGameMap :: GameMap -> Terrain -> [Coord]
fromGameMap gm t = let
  xy = Map.keys gm
  terrainList = filter ((==t).fst) $ terrainMap xy gm
  coordList = [v | (_, v) <- terrainList]
  in coordList

-- | fromHard list of Hard surfaces
fromHard :: GameMap -> [Coord]
fromHard gm = let
  xy = Map.keys gm
  terrainList = filter ((/=Open).fst) $ terrainMap xy gm
  coordList = [v | (_, v) <- terrainList]
  in coordList

-- | fromOpen list of Open surfaces
fromOpen :: GameMap -> [Coord]
fromOpen gm = let
  xy = Map.keys gm
  terrainList = filter ((==Open).fst) $ terrainMap xy gm
  coordList = [v | (_, v) <- terrainList]
  in coordList

listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  newGameData = GameData { visible = False, kind = k }
  in Map.insert v newGameData (listToMap xs)

-- | mkGrid helper function for zip
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

-- | mkGameMap builds the GameMap from Dungeon @d@
mkGameMap :: Dungeon -> GameMap
mkGameMap d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  terrainList = V.toList $ dungeonTiles d
  tileList = zip terrainList grid
  in listToMap tileList

-- | updateGameMap
-- just visible for now...
updateGameMap :: [Coord] -> GameMap -> GameMap
updateGameMap [] _      = Map.empty
updateGameMap (x:xs) gm = let
  Just k = Map.lookup x gm
  newGameData = GameData { visible = True, kind = (kind k) }
  in Map.insert x newGameData (updateGameMap xs gm)

-- | terrainMap returns @x:xs@ Terrain from GameMap
terrainMap :: [Coord] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  kinds = case Map.lookup x gm of
    Just k -> (kind k)
    _      -> Zero
  in [(kinds, x)] ++ terrainMap xs gm
