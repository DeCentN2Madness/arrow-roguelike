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
mkGameMap seed depth w h = let
  tm = cave seed w h
  in level depth w h tm

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

-- | level vaults
-- A version is door opening East...
-- B version is door opening West...
-- Sections:
-- 1 - 2 - 3
-- 4 - 5 - 6
-- 7 - 8 - 9
level :: Depth -> Int -> Int -> TileMap -> TileMap
level depth w h tm = let
  secW = w `div` 3
  secH = h `div` 3
  s1   = (secW-10, secH-10)
  s2   = (secW, secH-10)
  s3   = (2*secW, secH-10)
  s4   = (secW-10, secH)
  s5   = (secW, secH)
  s6   = (2*secW, secH)
  s7   = (secW-10, 2*secH-10)
  s8   = (secW, 2*secH-10)
  s9   = (2*secW, 2*secH-10)
  myLevel n
    | n > 18 =
      insertVaultPair s2 pillarA s9 pillarB $
      insertVaultPair s5 crossA  s6 crossB $
      insertVaultPair s8 townA   s3 townB tm
    | n > 16 && n <= 18 =
      insertVaultPair s2 townA   s9 townB $
      insertVaultPair s5 lairA   s6 lairB $
      insertVaultPair s7 pillarA s3 pillarB tm
    | n > 16 && n <= 18 =
      insertVaultPair s2 townA   s9 townB $
      insertVaultPair s4 crossA  s6 crossB $
      insertVaultPair s7 pillarA s3 pillarB tm
    | n > 12 && n <= 16 =
      insertVaultPair s1 pillarA s9 pillarB $
      insertVaultPair s4 crossA  s6 crossB $
      insertVaultPair s7 lairA   s3 lairB tm
    | n > 8 && n <= 12 =
      insertVaultPair s1 pillarA s9 pillarB $
      insertVaultPair s7 townA   s3 townB tm
    | n > 5 && n <= 8 =
      insertVaultPair s1 townA   s3 townB $
      insertVaultPair s4 lairA   s6 lairB tm
    | otherwise =
      insertVaultPair s1 lairA   s3 lairB tm
  in myLevel depth
