{-

Game.Kind.Cave.hs

Game.Kind.Cave is helper for Game.Vault for drawing various vaults

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Cave (cave
                      , crossA
                      , crossB
                      , mkGrid
                      , lairA
                      , lairB
                      , pillarA
                      , pillarB
                      , townA
                      , townB) where

import Prelude hiding (lookup)
import Control.Monad.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import qualified Game.Kind.Dungeon as GKD
import Game.Kind.Tile (Terrain(..), TileKind (..))
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type Coord = (Int, Int)
type Seed = Int

-- | add Terrain to TileMap at pos
-- (y,x) in [Terrain]
-- (x,y) in TileMap
add :: Coord -> [Terrain] -> TileMap -> TileMap
add (y1, x1) ts tm = let
  dist = length ts
  end = (dist + x1, dist + y1)
  coordList = mkGrid (x1, y1) end
  terrainList = zip coordList ts
  hallMap = Map.fromList $ zip coordList $
    [ tk | (xy, t) <- terrainList, let tk = TileKind xy False t ]
  finalMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy hallMap of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  in Map.fromList finalMap

-- | cave
-- rogueDungeon by reusing seed can regenerate dungeon
cave :: Seed -> Int -> Int -> TileMap
cave seed width height = let
  g = mkStdGen seed
  (d, _) = GKD.rogueDungeon width height g
  in GT.mkTileMap d

-- | uniform grid
mkGrid :: Coord -> Coord -> [Coord]
mkGrid (x1, y1) (x2, y2) = [ (y, x) | x <- [x1..x2], y <- [y1..y2] ]

-- | 1 x 4
board :: [Terrain]
board = replicate 4 Wall

-- | 1 x 2
spot :: [Terrain]
spot = replicate 2 Wall

-- | X
crossA :: TileMap
crossA = let
  t = lairA
  a = add (2,2) spot $ add (6,2) spot t
  b = add (3,3) board a
  c = add (4,4) spot b
  d = add (3,5) board c
  e = add (2,6) spot $ add (6,6) spot d
  in e

-- | X
crossB :: TileMap
crossB = let
  t = lairB
  a = add (2,2) spot $ add (6,2) spot t
  b = add (3,3) board a
  c = add (4,4) spot b
  d = add (3,5) board c
  e = add (2,6) spot $ add (6,6) spot d
  in e

-- | box
lairA :: TileMap
lairA = let
  t = GT.mkTileMap $ GKD.boxDungeon 10 10
  in add (9,5) [Door] t

-- | box
lairB :: TileMap
lairB = let
  t = GT.mkTileMap $ GKD.boxDungeon 10 10
  in add (0,5) [Door] t

-- | +
pillarA :: TileMap
pillarA = let
  t = lairA
  a = add (4,3) spot $ add (4,1) spot $ add (3,2) board t
  b = add (4,8) spot $ add (4,6) spot $ add (3,7) board a
  in b

-- | +
pillarB :: TileMap
pillarB = let
  t = lairB
  a = add (4,3) spot $ add (4,1) spot $ add (3,2) board t
  b = add (4,8) spot $ add (4,6) spot $ add (3,7) board a
  in b

-- | =
townA :: TileMap
townA = let
  t = lairA
  a = add (3,7) board  $ add (3,6) board t
  b = add (3,3) board  $ add (3,2) board a
  in b

-- | =
townB :: TileMap
townB = let
  t = lairB
  a = add (3,7) board  $ add (3,6) board t
  b = add (3,3) board  $ add (3,2) board a
  in b
