{-

Game.Tile.hs

Game.Tile is the engine for the TileKind.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Tile (fromMoveBlocked
                 , fromOpen
                 , fromVisual
                 , fromVisionBlocked
                 , TileMap
                 , mkTileMap
                 , updateTileMap) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Game.Kind.Tile

type Coord = (Int, Int)
type TileMap = Map Int TileKind

-- | dungeonGrid uniform grid helper function for zip
-- Note: Dungeon is Vector (width*height)
dungeonGrid :: Int -> Int -> [Coord]
dungeonGrid maxX maxY = let
  maxXY = if maxX > maxY then maxX else maxY
  in [ (y, x) | x <- [0..maxXY-1], y <- [0..maxXY-1] ]

-- | fromMoveBlocked filters movable coord
fromMoveBlocked :: [Coord] -> TileMap -> [Coord]
fromMoveBlocked pos tm = let
  terrainList = filter ((/=Door).fst) $
    filter ((/=Open).fst) $
    [ (t, xy) | (_, TileKind xy _ t _ _) <- Map.toList tm ]
  in filter (\x -> x `notElem` [ xy | (_, xy) <- terrainList ]) pos

-- | fromOpen list of Open surfaces
fromOpen :: TileMap -> [(Terrain, Coord)]
fromOpen tm = let
  terrainList = filter ((==Open).fst) $
    [ (t, xy) | (_, TileKind xy _ t _ _) <- Map.toList tm ]
  in terrainList

-- | fromVisual return visual terrain
fromVisual :: TileMap -> [(TileKind, Coord)]
fromVisual tm = let
  terrainList = filter ((/=(0,0)).snd) $
    [ (tk, xy) | (_, tk@(TileKind pos vis _ _ _)) <- Map.toList tm,
      let xy = if vis then pos else (0,0) ]
  in terrainList


-- | fromVisionBlocked returns VisionBlocked
fromVisionBlocked :: TileMap -> [(Terrain, Coord)]
fromVisionBlocked tm = let
  terrainList = filter ((/=Open).fst) $
    [ (t, xy) | (_, TileKind xy _ t _ _) <- Map.toList tm ]
  in terrainList

-- | mkTileMap builds the TileMap from Dungeon @d@
mkTileMap :: Dungeon -> TileMap
mkTileMap d = let
  grid = dungeonGrid (dungeonWidth d) (dungeonHeight d)
  tileList = V.toList $ dungeonTiles d
  terrainList = [ tk | (xy, t) <- zip grid tileList,
               let tk = TileKind xy False t (addVisual t) (addLit t) ]
  in Map.fromList $ zip [0 :: Int ..] terrainList

-- | updateTileMap
updateTileMap :: [Coord] -> TileMap -> TileMap
updateTileMap seen tm = let
  tileList = [ (ix, tk) | (ix, TileKind xy vis t vt vl) <- Map.toList tm,
               let tk = if xy `elem` seen
                     then TileKind xy True t vt vl
                     else TileKind xy vis  t vt vl ]
  in Map.fromList tileList
