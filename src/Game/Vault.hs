{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave
                  , insertVault
                  , level
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
cave :: Int -> Int -> Int -> TileMap
cave seed rows cols = let
  g = mkStdGen seed
  (d, _) = GD.rogueDungeon cols rows g
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

-- | insertVault at pos
-- Example: Game.Vault.insertVault (1,1) town dungeon
insertVault :: Coord -> TileMap -> TileMap -> TileMap
insertVault (startX, startY) vault tm = let
  -- insert Vault
  vaultMap = Map.fromList $
    [ (xy,  tk) | (_, TileKind (x, y) v t) <- Map.toList vault,
                 let tk = TileKind xy v t
                     xy = (startX + x, startY + y) ]
  updateMap = Map.fromList $
    [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy vaultMap of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  -- TODO calculate facing of door
  -- TODO handle multiple door and destinations
  finalMap = case doorList vaultMap of
    [] -> updateMap
    (x:_) -> let
      (doorX, doorY) = x
      newX = doorX + 10
      newY = doorY + 10
      horizontal = insertHall (doorX, newY) (newX, newY) updateMap
      in insertHall (doorX, doorY+1) (doorX, newY) horizontal
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
level :: TileMap -> TileMap
level tm = let
  l = lair
  p = pillar
  t = town
  in insertVault (1,1) t $ insertVault (10, 1) p $ insertVault (20,1) l tm

lair :: TileMap
lair = let
  t = cave 10 10 10
  in add (5,9) [Door] t

-- | pillar room
pillar :: TileMap
pillar = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (4,3) spot $ add (4,1) spot $ add (3,2) board t
  b = add (4,8) spot $ add (4,6) spot $ add (3,7) board a
  in add (5,9) [Door] b

-- | nice little town
town :: TileMap
town = let
  t = GT.mkTileMap $ GD.boxDungeon 10 10
  a = add (3,7) board  $ add (3,6) board t
  b = add (3,3) board  $ add (3,2) board a
  in add (5,9) [Door] b
