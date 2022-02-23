{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave
                  , insertVault
                  , showVault
                  , town) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.Random (mkStdGen)
import Data.List
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
add :: Coord -> [Terrain] -> TileMap -> TileMap
add (x1, y1) ts tm = let
  dist = length ts
  end = (dist + x1, dist + y1)
  coordList = mkGrid (x1, y1) end
  terrainList = zip coordList ts
  hallMap = Map.fromList $ zip coordList $ [ tk | (xy, t) <- terrainList,
                     let tk = TileKind xy False t]
  finalMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy hallMap of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  in Map.fromList finalMap

-- | 1 x 4
board :: [Terrain]
board = replicate 4 Wall

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
doorList :: TileMap -> [Coord]
doorList tm = nub $ filter (/=(0,0)) $
  [ xy | (_, TileKind pos _ t) <- Map.toList tm,
                let xy = if t == Door then pos else (0,0) ]
-- | insertHall
insertHall :: Coord -> Coord -> TileMap -> TileMap
insertHall src dest tm = let
  hallMap = mkHall src dest
  finalMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy hallMap of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  in Map.fromList finalMap

-- | insertVault at pos
-- draw Hall to middle of the Map from the door
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
  -- TODO find door for hall
  -- TODO calculate dest coords from door
  -- (y, x) coordinates
  hMap = insertHall (21,6) (21,41) updateMap
  vMap = insertHall (11,6) (21, 6) hMap
  in vMap


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
mkGrid (x1, y1) (x2, y2)= let
  in [(y, x) | x <- [x1..x2], y <- [y1..y2]]

-- | showVault for visualization
-- >>>  let d = cave 1 80 40
-- >>>      l = town
-- >>>  showVault $ insertVault (1,1) l d
showVault :: TileMap -> IO ()
showVault tm = do
  let txList = [ (xy, v) | (_, TileKind xy _ t) <- Map.toList tm,
                let v = terrainToText t ]
      doors  = doorList tm
  forM_ txList $ \((i,_), t) -> do
    let vt = if i == 0
          then T.append (T.pack "\n") t
          else t
    printf "%s" vt
  printf "\n"
  printf "door = %s\n" (show doors)

-- | TileMap to Text
terrainToText :: Terrain -> Text
terrainToText t = case t of
  Door -> "+"
  Magma -> "*"
  Open -> "."
  Rock -> ":"
  Rubble -> "%"
  Wall -> "#"

-- | nice little town
town :: TileMap
town = let
  d = GD.boxDungeon 10 10
  t = GT.mkTileMap d
  a = add (6,3) board  $ add (5,3) board t
  b = add (3,3) board  $ add (2,3) board a
  c = add (9,5) [Door] b
  in c
