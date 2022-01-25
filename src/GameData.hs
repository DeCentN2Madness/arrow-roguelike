{-

GameData.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(EntityMap
               , fromEntityMap
               , fromHard
               , fromVTerrain
               , getPlayer
               , getTerrainAt
               , GameMap
               , insertPlayer
               , insertMouse
               , insertMushroom
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

-- | terrainMap returns Terrain from GameMap
fromEntity :: [Int] -> EntityMap -> [(Entity, Coord)]
fromEntity [] _ = []
fromEntity (x:xs) em = let
  e = fromMaybe zeroEK $ Map.lookup x em
  in (eKind e, coord e) : fromEntity xs em

fromEntityMap :: EntityMap -> [(Entity, Coord)]
fromEntityMap em = let
  ks = Map.keys em
  in fromEntity ks em

-- | @ lives at 0
getPlayer :: EntityMap -> Coord
getPlayer em = let
  e = fromMaybe zeroEK $ Map.lookup 0 em
  in coord e

-- | getTerrainAt
getTerrainAt :: Coord -> GameMap -> Terrain
getTerrainAt k gm = let
  v = fromMaybe zeroTK $ Map.lookup k gm
  in tKind v

-- | fromHard list of Hard surfaces
fromHard :: GameMap -> [Coord]
fromHard gm = let
  ks = Map.keys gm
  terrainList = filter ((/=Open).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

-- | fromOpen list of Coord by Terrain @t@
fromOpen :: GameMap -> [(Terrain, Coord)]
fromOpen gm = let
  ks = Map.keys gm
  terrainList = filter ((==Open).fst) $ terrainMap ks gm
  in terrainList

-- | fromVisible list of Coord the hero has visited
fromVisible :: GameMap -> [Coord]
fromVisible gm = let
  ks = Map.keys gm
  terrainList = filter ((==True).fst) $ visibleMap ks gm
  in [v | (_, v) <- terrainList]

fromVTerrain :: GameMap -> [(Terrain, Coord)]
fromVTerrain gm = let
  ks = fromVisible gm
  in terrainMap ks gm

insertGameMap :: Coord -> TileKind -> GameMap -> GameMap
insertGameMap k v gm = case tKind v of
  ZeroT -> gm -- keep Zero out of GameMap
  _ -> Map.insert k v gm

-- insert @ into the GameMap
-- TODO Entity in EntityKind
insertPlayer :: GameMap ->  EntityMap -> EntityMap
insertPlayer gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Actor
  in Map.insert 0 (g { coord = openList!!2 }) em

-- insert r into the GameMap
-- TODO Entity in EntityKind
insertMouse :: GameMap ->  EntityMap -> EntityMap
insertMouse gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Mouse
  in Map.insert 1 (g { coord = openList!!4 }) em

-- insert r into the GameMap
-- TODO Entity in EntityKind
insertMushroom :: GameMap ->  EntityMap -> EntityMap
insertMushroom gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Mushroom
  in Map.insert 2 (g { coord = openList!!6 }) em

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
mkEntityMap :: EntityMap
mkEntityMap = Map.empty

-- | terrainMap returns Terrain from GameMap
terrainMap :: [Coord] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  kinds = case Map.lookup x gm of
    Just k -> tKind k
    _  -> ZeroT
  in (kinds, x) : terrainMap xs gm

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
  g = if tKind k /= ZeroT then k { visible = True } else k
  in g

-- | visibleMap returns Visible from GameMap
visibleMap :: [Coord] -> GameMap -> [(Bool, Coord)]
visibleMap [] _ = []
visibleMap (x:xs) gm = let
  v = case Map.lookup x gm of
    Just k -> visible k
    _      -> False
  in (v, x) : visibleMap xs gm
