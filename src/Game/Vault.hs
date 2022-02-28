{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Game.Vault creates the levels for the Dungeon

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave
                  , mkGameMap
                  , showVault) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.Random (mkStdGen)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import qualified Game.Dungeon as GD
import Game.Kind.Tile (Terrain(..), TileKind (..))
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type Coord = (Int, Int)
type CoordMap = Map Coord TileKind
type Depth = Int
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
  (d, _) = GD.rogueDungeon width height g
  in GT.mkTileMap d

-- | chessDist - Chess distance between two points.
chessDist :: Coord -> Coord -> Int
{-# INLINE chessDist #-}
chessDist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

-- | doorList
doorList :: CoordMap -> [Coord]
doorList tm = filter (/=(0,0)) $
  [ xy | (_, TileKind pos _ t) <- Map.toList tm,
                let xy = if t == Door then pos else (0,0) ]

-- | insertHall
-- (y,x) in [Terrain]
-- (x,y) in TileMap
insertHall :: Coord -> Coord -> TileMap -> TileMap
insertHall (y1, x1) (y2, x2) tm = let
  hallMap = mkHall (x1, y1) (x2, y2)
  finalMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy hallMap of
                      Just x -> x
                      Nothing -> TileKind xy v t ]
  in Map.fromList finalMap

-- | insertVaultPair
-- insert vaults in pairs and link the doors with vertical and
-- horizontal hallways
-- Example: insertVaultPair (1,1) townA (70,30) townB
insertVaultPair :: Coord -> TileMap -> Coord -> TileMap -> TileMap -> TileMap
insertVaultPair (x1, y1) vault1 (x2, y2) vault2 tm = let
  -- insert Vault
  vaultA = Map.fromList $
    [ (xy,  tk) | (_, TileKind (x, y) v t) <- Map.toList vault1,
                 let tk = TileKind xy v t
                     xy = (x1 + x, y1 + y) ]
  vaultB = Map.fromList $
    [ (xy,  tk) | (_, TileKind (x, y) v t) <- Map.toList vault2,
                 let tk = TileKind xy v t
                     xy = (x2 + x, y2 + y) ]
  -- update TileMap
  updateA = Map.fromList $
    [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy vaultA of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  updateB = Map.fromList $
    [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList updateA,
                let tk = case Map.lookup xy vaultB of
                      Just x -> x
                      Nothing -> TileKind xy v t]
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

-- | mkHall
mkHall :: Coord -> Coord -> CoordMap
mkHall (x1, y1) (x2, y2) = let
  dist = chessDist (x1, y1) (x2, y2)
  ts   = replicate dist Open
  coordList = mkGrid (x1, y1) (x2, y2)
  terrainList = zip coordList ts
  tm = zip coordList $ [ tk | (xy, t) <- terrainList,
                     let tk = TileKind xy False t]
  in Map.fromList tm

-- | mkGameMap
-- GameMap influenced by depth
-- TODO depth, player level, so on...
mkGameMap :: Seed -> Depth -> Int -> Int -> TileMap
mkGameMap seed depth width height= let
  tm = cave seed width height
  in level depth tm

-- | uniform grid
mkGrid :: Coord -> Coord -> [Coord]
mkGrid (x1, y1) (x2, y2) = [ (y, x) | x <- [x1..x2], y <- [y1..y2] ]

-- | showVault for visualization
-- >>>  let d = cave 1 80 40
-- >>>      l = town
-- >>>  showVault $ insertVault (1,1) l d
showVault :: TileMap -> IO ()
showVault tm = do
  let txList = [ (xy, v) | (_, TileKind xy _ t) <- Map.toList tm,
                let v = terrainToText t ]
  forM_ txList $ \((i,_), t) -> do
    let vt = if i == 0
          then T.append (T.pack "\n") t
          else t
    printf "%s" vt
  printf "\n"

-- | TileMap to Text
terrainToText :: Terrain -> Text
terrainToText t = case t of
  Door -> "+"
  Magma -> "*"
  Open -> "."
  Rock -> ":"
  Rubble -> "%"
  Wall -> "#"

-- | 1 x 4
board :: [Terrain]
board = replicate 4 Wall

-- | 1 x 2
spot :: [Terrain]
spot = replicate 2 Wall

-- | demo rooms
-- A version is door opening East,
-- B version is door opening West...
level :: Depth -> TileMap -> TileMap
level depth tm = let
  l0 = lairA
  l1 = lairB
  p0 = pillarA
  p1 = pillarB
  t0 = townA
  t1 = townB
  finalMap = if depth > 0
    then if depth > 10
    then insertVaultPair (1,30) t0 (70,1) l1 $ -- Hard
    insertVaultPair (1,15) p0 (70,15) t1 $
    insertVaultPair (1,1) l0 (70,30) p1 tm
    else insertVaultPair (1,1) t0 (70,1) t1 $ -- Medium
    insertVaultPair (1,15) p0 (70,25) p1 tm
    else insertVaultPair (1,1) t0 (30,1) t1 tm -- Easy
  in finalMap

lairA :: TileMap
lairA = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  in add (9,5) [Door] t

lairB :: TileMap
lairB = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  in add (0,5) [Door] t

-- | pillar room
pillarA :: TileMap
pillarA = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (4,3) spot $ add (4,1) spot $ add (3,2) board t
  b = add (4,8) spot $ add (4,6) spot $ add (3,7) board a
  in add (9,5) [Door] b

-- | pillar room
pillarB :: TileMap
pillarB = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (4,3) spot $ add (4,1) spot $ add (3,2) board t
  b = add (4,8) spot $ add (4,6) spot $ add (3,7) board a
  in add (0,5) [Door] b

-- | nice little town
townA :: TileMap
townA = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (3,7) board  $ add (3,6) board t
  b = add (3,3) board  $ add (3,2) board a
  in add (9,5) [Door] b

-- | nice little town
townB :: TileMap
townB = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (3,7) board  $ add (3,6) board t
  b = add (3,3) board  $ add (3,2) board a
  in add (0,5) [Door] b
