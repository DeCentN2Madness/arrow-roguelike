{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(fromGameMap
               , fromHard
               , fromOpen
               , fromVisible
               , GameData
               , GameMap
               , mkGameMap
               , updateGameMap
               , unionGameMap) where

import qualified Data.Map as Map
import Data.Maybe
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

-- | fromVisible list of Coord the hero has visited
fromVisible :: GameMap -> [Coord]
fromVisible gm = let
  xy = Map.keys gm
  terrainList = filter ((==True).fst) $ visibleMap xy gm
  coordList = [v | (_, v) <- terrainList]
  in coordList

-- | listToMap builds the GameMap
listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  g = GameData { visible = False, kind = k }
  in Map.insert v g (listToMap xs)

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

-- | terrainMap returns @x:xs@ Terrain from GameMap
terrainMap :: [Coord] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  kinds = case Map.lookup x gm of
    Just k -> (kind k)
    _      -> Zero
  in [(kinds, x)] ++ terrainMap xs gm

-- | updateVisible
updateVisible :: Coord -> GameMap -> GameData
updateVisible x gm = let
  k = fromMaybe zeroG $ Map.lookup x gm
  g = if kind k /= Zero then k { visible = True } else k
  in g

-- | updateGameMap
-- just visible for now...
updateGameMap :: Coord -> GameMap -> GameMap
updateGameMap k gm = let
  v = updateVisible k gm
  g = if kind v /= Zero then Map.insert k v gm else gm
  in g

-- | unionGameMap helper for union
unionGameMap :: GameMap -> GameMap -> GameMap
unionGameMap new old = Map.union new old

-- | visibleList returns @x:xs@ Terrain from GameMap
visibleMap :: [Coord] -> GameMap -> [(Bool, Coord)]
visibleMap [] _ = []
visibleMap (x:xs) gm = let
  vis = case Map.lookup x gm of
    Just k -> (visible k)
    _      -> False
  in [(vis, x)] ++ visibleMap xs gm

-- | zeroG useful for filter
zeroG :: GameData
zeroG = GameData False Zero
