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
type GameMap = Map Int TileKind
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
  terrainList = [ (pos, t) | (_, tk) <- Map.toList gm,
                  let TileKind pos _ t = tk ]
  in snd $ head $ filter ((==k).fst) terrainList

-- | fromHard list of Hard surfaces
fromHard :: GameMap -> [Coord]
fromHard gm = let
  ks = Map.keys gm
  terrainList = filter ((/=Open).fst) $ terrainMap ks gm
  in [v | (_, v) <- terrainList]

-- | fromOpen list of Open surfaces
fromOpen :: GameMap -> [(Terrain, Coord)]
fromOpen gm = let
  ks = Map.keys gm
  terrainList = filter ((==Open).fst) $ terrainMap ks gm
  in terrainList

-- | fromVTerrain return visual terrain
fromVTerrain :: GameMap -> [(Terrain, Coord)]
fromVTerrain gm = let
  visualList = [ (t, xy) | (_, TileKind pos vis t) <- Map.toList gm,
                 let xy = if vis then pos else (0,0)]
  in visualList

-- | insert @ into the GameMap
insertPlayer :: GameMap ->  EntityMap -> EntityMap
insertPlayer gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Actor
  in Map.insert 0 (g { coord = openList!!2 }) em

-- | insert r into the GameMap
insertMouse :: GameMap ->  EntityMap -> EntityMap
insertMouse gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Mouse
  in Map.insert 1 (g { coord = openList!!4 }) em

-- | insert , into the GameMap
insertMushroom :: GameMap ->  EntityMap -> EntityMap
insertMushroom gm em = let
  openList = [ v | (_, v) <- fromOpen gm ]
  g = mkEntity Mushroom
  in Map.insert 2 (g { coord = openList!!6 }) em

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
  gameList = [ v | (i, j) <- tileList, let v = TileKind j False i]
  gm = zip [0 :: Int ..] gameList
  in Map.fromList gm

-- | mkEntityMap will do more
mkEntityMap :: EntityMap
mkEntityMap = Map.empty

-- | terrainMap returns Terrain from GameMap
terrainMap :: [Int] -> GameMap -> [(Terrain, Coord)]
terrainMap [] _ = []
terrainMap (x:xs) gm = let
  (TileKind pos _ t) = fromMaybe zeroTK $ Map.lookup x gm
  in (t, pos) : terrainMap xs gm

-- | updateGameMap
-- just visible for now...
updateGameMap :: [Coord] -> GameMap -> GameMap
updateGameMap [] gm = gm
updateGameMap (x:xs) gm = let
  ixList = [(i, pos) | (i, TileKind pos _ _) <- Map.toList gm]
  ix = case filter ((==x).snd) ixList of
    [k] -> fst k
    _ -> 0
  TileKind xy _ t = fromMaybe zeroTK $ Map.lookup ix gm
  in updateGameMap xs (Map.insert ix (TileKind xy True t) gm)

-- update @ position
updatePlayer :: Coord -> EntityMap -> EntityMap
updatePlayer v em = let
  e = fromMaybe zeroEK $ Map.lookup 0 em
  in Map.insert 0 (e { coord = v }) em
