{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(EntityMap
               , findPlayer
               , fromHard
               , fromVTerrain
               , GameMap
               , insertPlayer
               , mkEntityMap
               , mkGameMap
               , updateGameMap
               , updatePlayer) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Vector as V
import Dungeon (Dungeon(..), Terrain(..))
import EntityKind
import TileKind

type Coord = (Int, Int)
type GameMap = Map Coord TileKind
type EntityMap = Map Int EntityKind

-- | @ lives at 0
findPlayer :: EntityMap -> Coord
findPlayer em = let
  e = fromMaybe zeroEK $ Map.lookup 0 em
  in coord e

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
insertGameMap k v gm = case tKind v of
  Zero -> gm -- keep Zero out of GameMap
  _ -> Map.insert k v gm

-- | insertEntityMap
-- TODO Entity in EntityKind
insertEntityMap :: Int -> EntityKind -> EntityMap -> EntityMap
insertEntityMap k v em = Map.insert k v em

-- insert @ into the GameMap
-- TODO Entity in EntityKind
insertPlayer :: GameMap -> EntityMap -> EntityMap
insertPlayer gm em = let
  openList = fromOpen gm
  pos = openList!!0
  v = EntityKind { coord = pos
                 , eKind = Player
                 , desc = "the Hero"
                 , name = "Player"
                 , block = True }
  in insertEntityMap 0 v em

-- | listToMap builds the GameMap
listToMap :: [(Terrain, Coord)] -> GameMap
listToMap []         = Map.empty
listToMap ((k,v):xs) = let
  g = TileKind { location = v, visible = False, tKind = k }
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
    Just k -> (tKind k)
    _      -> Zero
  in [(kinds, x)] ++ terrainMap xs gm

-- | updateGameMap
-- just visible for now...
updateGameMap :: [Coord] -> GameMap -> GameMap
updateGameMap [] gm = gm
updateGameMap (x:xs) gm = let
  g = updateVisible x gm
  in updateGameMap xs (insertGameMap x g gm)

-- update @ position
updatePlayer :: Coord -> EntityMap -> EntityMap
updatePlayer v em = let
  e = fromMaybe zeroEK $ Map.lookup 0 em
  in Map.insert 0 (e { coord = v }) em

-- | updateVisible
updateVisible :: Coord -> GameMap -> TileKind
updateVisible x gm = let
  k = fromMaybe zeroTK $ Map.lookup x gm
  g = if (tKind k) /= Zero then k { visible = True } else k
  in g

-- | visibleMap returns Visible from GameMap
visibleMap :: [Coord] -> GameMap -> [(Bool, Coord)]
visibleMap [] _ = []
visibleMap (x:xs) gm = let
  v = case Map.lookup x gm of
    Just k -> (visible k)
    _      -> False
  in [(v, x)] ++ visibleMap xs gm
