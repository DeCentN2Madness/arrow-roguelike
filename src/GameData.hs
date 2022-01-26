{-

GameData.hs

GameData is the engine for the game Kinds and returns from the map
in pattern of [(Kind, Coord)]

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module GameData(EntityMap
               , fromBlock
               , fromEntity
               , fromHard
               , fromOpen
               , fromVisual
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

-- | fromBlock
fromBlock :: EntityMap -> [(Int, Coord)]
fromBlock em = let
  entityList = [ (i, xy) | (i, ek) <- Map.toList em,
                 let xy = if block ek then coord ek else (0,0) ]
  in entityList

-- | fromEntity in the World
fromEntity :: EntityMap -> [(Entity, Coord)]
fromEntity em = let
  entityList = [ (t, pos) | (_, ek) <- Map.toList em,
                 let pos = coord ek
                     t = eKind ek ]
  in entityList

-- | @ lives at 0
getPlayer :: EntityMap -> Coord
getPlayer em = let
  entityList = [ xy | (t, pos) <- fromEntity em,
                 let xy = if t == Actor then pos else (0,0)]
  in head $ filter (/=(0,0)) entityList

-- | getTerrainAt
getTerrainAt :: Coord -> GameMap -> Terrain
getTerrainAt k gm = let
  terrainList = [ (pos, t) | (_, tk) <- Map.toList gm,
                  let TileKind pos _ t = tk ]
  in snd $ head $ filter ((==k).fst) terrainList

-- | fromHard list of Hard surfaces
fromHard :: GameMap -> [(Terrain, Coord)]
fromHard gm = let
  terrainList = [ (t, pos) | (_, TileKind pos _ t) <- Map.toList gm ]
  in filter ((/=Open).fst) terrainList

-- | fromOpen list of Open surfaces
fromOpen :: GameMap -> [(Terrain, Coord)]
fromOpen gm = let
  terrainList = [ (t, pos) | (_, TileKind pos _ t) <- Map.toList gm ]
  in filter ((==Open).fst) terrainList

-- | fromVisual return visual terrain
fromVisual :: GameMap -> [(Terrain, Coord)]
fromVisual gm = let
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
