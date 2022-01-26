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
               , getEntityAt
               , getTerrainAt
               , GameMap
               , insertMouse
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
  e = getEntityAt 0 em
  in coord e

getEntityAt :: Int -> EntityMap -> EntityKind
getEntityAt x em = fromMaybe zeroEK $ Map.lookup x em

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

insertEntity :: Int -> Coord -> Entity -> EntityMap -> EntityMap
insertEntity k pos e em = let
  ek = mkEntity e pos
  in Map.insert k ek em

-- | insert @ into the GameMap
insertPlayer :: GameMap -> EntityMap -> EntityMap
insertPlayer gm em = let
  openList = [ v | (_, v) <- fromOpen gm]
  xy = head openList
  in insertEntity 0 xy Actor em

insertMouse :: GameMap -> EntityMap
insertMouse gm = let
  openList = tail $ [ v | (_, v) <- fromOpen gm ]
  miceList = [ m | v <- openList, let m = mkEntity Mouse v]
  mice = zip [1..3] miceList
  in Map.fromList mice

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
