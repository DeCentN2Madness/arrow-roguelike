{-

Game.Tile.hs

Game.Tile is the engine for the Tile Kind and mostly returns from the map
in pattern of [(Kind, Coord)]

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Tile(fromHard
               , fromOpen
               , fromVisual
               , getTerrainAt
               , TileMap
               , mkTileMap
               , updateTileMap) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Vector as V
import Game.Dungeon (Dungeon(..), Terrain(..))
import Game.Kind.Tile

type Coord = (Int, Int)
type TileMap = Map Int TileKind


-- | getTerrainAt
getTerrainAt :: Coord -> TileMap -> Terrain
getTerrainAt k gm = let
  terrainList = [ (pos, t) | (_, tk) <- Map.toList gm,
                  let TileKind pos _ t = tk ]
  in snd $ head $ filter ((==k).fst) terrainList

-- | fromHard list of Hard surfaces
fromHard :: TileMap -> [(Terrain, Coord)]
fromHard gm = let
  terrainList = [ (t, pos) | (_, TileKind pos _ t) <- Map.toList gm ]
  in filter ((/=Open).fst) terrainList

-- | fromOpen list of Open surfaces
fromOpen :: TileMap -> [(Terrain, Coord)]
fromOpen gm = let
  terrainList = [ (t, pos) | (_, TileKind pos _ t) <- Map.toList gm ]
  in filter ((==Open).fst) terrainList

-- | fromVisual return visual terrain
fromVisual :: TileMap -> [(Terrain, Coord)]
fromVisual gm = let
  visualList = [ (t, xy) | (_, TileKind pos vis t) <- Map.toList gm,
                 let xy = if vis then pos else (0,0)]
  in visualList

-- | mkGrid helper function for zip
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

-- | mkTileMap builds the TileMap from Dungeon @d@
mkTileMap :: Dungeon -> TileMap
mkTileMap d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  terrainList = V.toList $ dungeonTiles d
  tileList = zip terrainList grid
  gameList = [ v | (i, j) <- tileList, let v = TileKind j False i]
  gm = zip [0 :: Int ..] gameList
  in Map.fromList gm

-- | updateTileMap
-- foldr mkTile
-- just visible for now...
updateTileMap :: [Coord] -> TileMap -> TileMap
updateTileMap [] gm = gm
updateTileMap (x:xs) gm = let
  ixList = [(i, pos) | (i, TileKind pos _ _) <- Map.toList gm]
  ix = case filter ((==x).snd) ixList of
    [k] -> fst k
    _ -> 0
  TileKind xy _ t = fromMaybe zeroTK $ Map.lookup ix gm
  in updateTileMap xs (Map.insert ix (TileKind xy True t) gm)
