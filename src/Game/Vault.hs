{-

Game.Vault.hs

Game.Vault creates the Level for the World

Usage:
  mkGameMap is the World Level

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave, mkGameMap) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Game.Kind.Cave
import Game.Kind.Tile
import Game.Tile (TileMap)

type Coord = (Int, Int)
type CoordMap = Map Coord TileKind
type Depth = Int
type Seed = Int

-- | chessDist - Chess distance between two points.
chessDist :: Coord -> Coord -> Int
{-# INLINE chessDist #-}
chessDist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

-- | doorList
doorList :: CoordMap -> [Coord]
doorList tm = filter (/=(0,0)) $
  [ xy | (_, TileKind pos _ t _ _) <- Map.toList tm,
                let xy = if t == Door then pos else (0,0) ]

-- | insertHall
-- (y,x) in [Terrain]
-- (x,y) in TileMap
insertHall :: Coord -> Coord -> TileMap -> TileMap
insertHall (y1, x1) (y2, x2) tm = let
  hallMap = mkHall (x1, y1) (x2, y2)
  finalMap = [ (ix, tk) | (ix, t@(TileKind xy _ _ _ _)) <- Map.toList tm,
      let tk = fromMaybe t (Map.lookup xy hallMap) ]
  in Map.fromList finalMap

{-
-- | insertVault
insertVault :: Coord -> TileMap -> TileMap -> TileMap
insertVault (x1, y1) vault tm = let
  vaultA = Map.fromList $
    [ (xy, tk) | (_, TileKind (x, y) v t vt vl) <- Map.toList vault,
      let tk = TileKind xy v t vt vl
          xy = (x1 + x, y1 + y) ]
  updateT = [ (ix, tk) | (ix, t@(TileKind xy _ _ _ _)) <- Map.toList tm,
      let tk = fromMaybe t (Map.lookup xy vaultA) ]
  in Map.fromList updateT
-}

-- | insertVaultPair
-- insert vaults in pairs and link the doors with vertical and
-- horizontal hallways
-- Example: insertVaultPair (1,1) townA (70,30) townB
insertVaultPair :: Coord -> TileMap -> Coord -> TileMap -> TileMap -> TileMap
insertVaultPair (x1, y1) v1 (x2, y2) v2 tm = let
  -- insert Vault
  vaultA = Map.fromList $
    [ (xy, tk) | (_, TileKind (x, y) v t vt vl) <- Map.toList v1,
      let tk = TileKind xy v t vt vl
          xy = (x1 + x, y1 + y) ]
  vaultB = Map.fromList $
    [ (xy, tk) | (_, TileKind (x, y) v t vt vl) <- Map.toList v2,
      let tk = TileKind xy v t vt vl
          xy = (x2 + x, y2 + y) ]
  -- update TileMap
  updateA = Map.fromList $
    [ (ix, tk) | (ix, t@(TileKind xy _ _ _ _)) <- Map.toList tm,
      let tk = fromMaybe t (Map.lookup xy vaultA) ]
  updateB = Map.fromList $
    [ (ix, tk) | (ix, t@(TileKind xy _ _ _ _)) <- Map.toList updateA,
      let tk = fromMaybe t (Map.lookup xy vaultB) ]
  -- link doors with vertical and horizontal hall
  finalMap = case doorList vaultA of
    [] -> updateB
    (x:_) -> let
      (doorX, doorY) = x
      (newX, newY) = case doorList vaultB of
        [] -> (doorX, doorY)
        (y:_) -> y
      horizontal = insertHall (doorX+1, newY) (newX, newY) updateB
      in insertHall (doorX+1, doorY) (doorX+1, newY) horizontal
  in finalMap

-- | mkGameMap
-- GameMap influenced by depth
mkGameMap :: Seed -> Depth -> Int -> Int -> TileMap
mkGameMap seed depth width height= let
  tm = cave seed width height
  in level depth tm

-- | mkHall
mkHall :: Coord -> Coord -> CoordMap
mkHall (x1, y1) (x2, y2) = let
  dist = chessDist (x1, y1) (x2, y2)
  ts   = replicate dist Open
  coordList = mkGrid (x1, y1) (x2, y2)
  terrainList = zip coordList ts
  tm = zip coordList $ [ tk | (xy, t) <- terrainList,
                     let tk = TileKind xy False t (addVisual t) (addLit t) ]
  in Map.fromList tm

-- | demo vaults
-- A version is door opening East,
-- B version is door opening West...
level :: Depth -> TileMap -> TileMap
level depth tm
  | depth > 10 = insertVaultPair (1,30) townA (70,1) townB $
    insertVaultPair (1,15) crossA (70,15) crossB $
    insertVaultPair (1,1) pillarA (70,30 ) pillarB tm
  | depth > 5 && depth <= 10 = insertVaultPair (1,1) crossA (70,1) crossB $
    insertVaultPair (1,15) townA (70,25) townB tm
  | otherwise = insertVaultPair (1,1) townA (30,1) townB tm  -- Easy
