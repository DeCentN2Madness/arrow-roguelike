{-

Game.Tile.hs

Game.Tile is the engine for the Tile Kind and mostly returns from the map
in pattern of [(Terrain, Coord)]

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
import qualified Data.Vector as V
import Game.Dungeon (Dungeon(..))
import Game.Kind.Tile

type Coord = (Int, Int)
type TileMap = Map Int TileKind

-- | fromHard list of Hard surfaces
fromHard :: TileMap -> [(Terrain, Coord)]
fromHard tm = let
  terrainList = [ (t, xy) | (_, TileKind xy _ t) <- Map.toList tm ]
  in filter ((/=Open).fst) terrainList

-- | fromOpen list of Open surfaces
fromOpen :: TileMap -> [(Terrain, Coord)]
fromOpen tm = let
  terrainList = [ (t, xy) | (_, TileKind xy _ t) <- Map.toList tm ]
  in filter ((==Open).fst) terrainList

-- | fromVisual return visual terrain
fromVisual :: TileMap -> [(Terrain, Coord)]
fromVisual tm = let
  visualList = [ (t, xy) | (_, TileKind pos vis t) <- Map.toList tm,
                 let xy = if vis then pos else (0,0)]
  in visualList

-- | getTerrainAt
getTerrainAt :: Coord -> TileMap -> Terrain
getTerrainAt pos tm = let
  terrainList = [ (xy, t) | (_, tk) <- Map.toList tm,
                  let TileKind xy _ t = tk ]
  in snd $ head $ filter ((==pos).fst) terrainList

-- | mkGrid uniform grid helper function for zip
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [(y, x)| x <- [0..maxXY-1], y <- [0..maxXY-1]]

-- | mkTileMap builds the TileMap from Dungeon @d@
mkTileMap :: Dungeon -> TileMap
mkTileMap d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  terrainList = V.toList $ dungeonTiles d
  tileList = [ tk | (t, xy) <- zip terrainList grid,
               let tk = TileKind xy False t]
  tm = zip [0 :: Int ..] tileList
  in Map.fromList tm

-- | updateTileMap
-- just visible for now...
updateTileMap :: [Coord] -> TileMap -> TileMap
updateTileMap [] tm = tm
updateTileMap (x:xs) tm = let
  ixList = [(i, pos) | (i, TileKind pos _ _) <- Map.toList tm]
  ix = case filter ((==x).snd) ixList of
    [k] -> fst k
    _ -> 0
  TileKind xy _ t = Map.findWithDefault (TileKind (0,0) False Open) ix tm
  in updateTileMap xs (Map.insert ix (TileKind xy True t) tm)
