{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(EntityMap
               , fromHard
               , fromVTerrain
               , GameMap
               , insertPlayer
               , mkEntityMap
               , mkGameMap
               , updateGameMap) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Vector as V
import Dungeon (Dungeon(..), Terrain(..))

type Coord = (Int, Int)

data EntityKind = EntityKind
  { coord :: Coord
  , code :: String
  , desc :: String
  , name :: String
  , block :: Bool
  } deriving (Show)

type GameMap = Map Coord TileKind

type EntityMap = Map Int EntityKind

data TileKind = TileKind
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
fromOpen gm = fromTerrain gm Open

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

insertGameMap :: Coord -> TileKind -> GameMap -> GameMap
insertGameMap k v gm = case kind v of
  Zero -> gm -- keep Zero out of GameMap
  _ -> Map.insert k v gm

-- | insertEntityMap
-- TODO Entity in EntityKind
insertEntityMap :: Int -> EntityKind -> EntityMap -> EntityMap
insertEntityMap k v em = Map.insert k v em

-- insert @ into the GameMap
-- TODO Entity in EntityKind
insertPlayer :: GameMap -> EntityMap -> (Coord, EntityMap)
insertPlayer gm em = let
  openList = fromOpen gm
  pos = openList!!0
  v = EntityKind { coord = pos
                 , code = "@"
                 , desc = "the Hero"
                 , name = "Player"
                 , block = True }
  e = insertEntityMap 0 v em
  in (pos, e)

-- | listToMap builds the GameMap
listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  g = TileKind { visible = False, kind = k }
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

-- | mkEntityMap will do more
-- TODO Entity in EntityKind
mkEntityMap :: EntityMap
mkEntityMap = Map.empty

-- | terrainMap returns Terrain from GameMap
terrainMap :: [Coord] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  kinds = case Map.lookup x gm of
    Just k -> (kind k)
    _      -> Zero
  in [(kinds, x)] ++ terrainMap xs gm

-- | updateVisible
updateVisible :: Coord -> GameMap -> TileKind
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
  in updateGameMap xs (insertGameMap x g gm)

-- | visibleMap returns Visible from GameMap
visibleMap :: [Coord] -> GameMap -> [(Bool, Coord)]
visibleMap [] _ = []
visibleMap (x:xs) gm = let
  v = case Map.lookup x gm of
    Just k -> (visible k)
    _      -> False
  in [(v, x)] ++ visibleMap xs gm

-- | zeroG useful for filter
zeroG :: TileKind
zeroG = TileKind False Zero
