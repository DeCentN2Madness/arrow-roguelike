{-

Game.Tile.hs

Game.Tile is the engine for the Tile Kind and returns from the Map
w/ [(Terrain, Coord)]

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Tile (fromHard
               , fromOpen
               , fromVisual
               , fromVisionBlocked
               , getTerrainAt
               , TileMap
               , mkTileMap
               , updateTileMap) where

import Prelude hiding (lookup)
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
  in filter ((/=Door).fst) $ filter ((/=Open).fst) terrainList

-- | fromOpen list of Open surfaces
fromOpen :: TileMap -> [(Terrain, Coord)]
fromOpen tm = let
  terrainList = [ (t, xy) | (_, TileKind xy _ t) <- Map.toList tm ]
  in filter ((==Open).fst) terrainList

-- | fromVisual return visual terrain
fromVisual :: TileMap -> [(Terrain, Coord)]
fromVisual tm = let
  terrainList = [ (t, xy) | (_, TileKind pos vis t) <- Map.toList tm,
                 let xy = if vis then pos else (0,0) ]
  in filter ((/=(0,0)).snd) terrainList

-- | fromVisionBlocked returns VisionBlocked
fromVisionBlocked :: TileMap -> [(Terrain, Coord)]
fromVisionBlocked tm = let
  terrainList = [ (t, xy) | (_, TileKind xy _ t) <- Map.toList tm ]
  in filter ((/=Open).fst) terrainList

-- | getTerrainAt
getTerrainAt :: Coord -> TileMap -> Terrain
getTerrainAt pos tm = let
  terrainList = [ (xy, t) | (_, tk) <- Map.toList tm,
                  let TileKind xy _ t = tk ]
  in snd $ head $ filter ((==pos).fst) terrainList

-- | mkGrid uniform grid helper function for zip
mkGrid :: Int -> Int -> [Coord]
mkGrid maxX maxY = [ (y, x) | x <- [0..maxX-1], y <- [0..maxY-1] ]

-- | mkTileMap builds the TileMap from Dungeon @d@
-- (y,x) in Terrain
-- (x,y) in TileMap
mkTileMap :: Dungeon -> TileMap
mkTileMap d = let
  grid = mkGrid (dungeonWidth d) (dungeonHeight d)
  tileList = V.toList $ dungeonTiles d
  terrainList = [ tk | (t, xy) <- zip tileList grid,
               let tk = TileKind xy False t]
  tm = Map.fromList $ zip [0 :: Int ..] terrainList
  in tm

-- | updateTileMap
updateTileMap :: [Coord] -> TileMap -> TileMap
updateTileMap seen tm = let
  tileList = [ (ix, tk) | (ix, TileKind xy vis t) <- Map.toList tm,
               let tk = if xy `elem` seen
                     then TileKind xy True t
                     else TileKind xy vis  t ]
  in Map.fromList tileList
