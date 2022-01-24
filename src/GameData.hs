{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(fromHard
               , fromOpen
               , fromTerrain
               , fromVisible
               , fromVTerrain
               , GameData
               , GameMap
               , mkGameMap
               , updateGameMap
               , unionGameMap
               , unionsGameMap) where

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

-- | fromHard list of Hard surfaces
fromHard :: GameMap -> [Coord]
fromHard gm = let
  ks = Map.keys gm
  terrainList = filter ((/=Open).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

-- | fromOpen list of Open surfaces
fromOpen :: GameMap -> [Coord]
fromOpen gm = let
  ks = Map.keys gm
  terrainList = filter ((==Open).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

-- | fromTerrain list of Coord by Terrain @t@
fromTerrain :: GameMap -> Terrain -> [Coord]
fromTerrain gm t = let
  ks = Map.keys gm
  terrainList = filter ((==t).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

-- | fromVisible list of Coord the hero has visited
fromVisible :: GameMap -> [Coord]
fromVisible gm = let
  ks = Map.keys gm
  terrainList = filter ((==True).fst) $ visibleMap ks gm
  in [v | (_, v) <- terrainList]

fromVTerrain :: GameMap -> Terrain -> [Coord]
fromVTerrain gm t = let
  ks = fromVisible gm
  terrainList = filter((==t).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

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

-- | terrainMap returns Terrain from GameMap
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
  g = if (kind k) /= Zero then k { visible = True } else k
  in g

-- | updateGameMap
-- just visible for now...
updateGameMap :: [Coord] -> GameMap -> GameMap
updateGameMap [] gm = gm
updateGameMap (x:xs) gm = let
  g = updateVisible x gm
  in updateGameMap xs (Map.insert x g gm)

-- | unionGameMap helper for union
unionGameMap :: GameMap -> GameMap -> GameMap
unionGameMap new old = Map.union new old

unionsGameMap :: [GameMap] -> GameMap
unionsGameMap xs = Map.unions xs

-- | visibleMap returns Visible from GameMap
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
