{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave
                  , insertVault
                  , lair
                  , showVault
                  , town) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import qualified Game.Dungeon as GD
import Game.Kind.Tile (Terrain(..), TileKind (..))
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type Coord = (Int, Int)

-- | add Terrain to TileMap at pos
add :: Coord -> [Terrain] -> TileMap -> TileMap
add pos ts tm = let
  coordList = mkGrid pos (length ts) (length ts)
  terrainList = zip coordList ts
  ixList = filter (/=(-1)) $ [ i | (ix, TileKind xy _ _) <- Map.toList tm,
             let i = if xy `elem` coordList then ix else (-1) ]
  terrainMap = Map.fromList $ zip ixList $ [ v | (xy, t) <- terrainList,
               let v = TileKind xy True t ]
  in Map.union terrainMap tm

-- | 1 x 4
board :: [Terrain]
board = replicate 4 Wall

-- | cave
-- rogueDungeon by reusing seed can regenerate dungeon
cave :: Int -> Int -> Int -> TileMap
cave seed rows cols = let
  g = mkStdGen (seed*rows*cols)
  (d, _) = GD.rogueDungeon cols rows g
  in GT.mkTileMap d

-- | TileMap to Text
drawVault :: TileMap -> [Text]
drawVault tm = let
  textList = [ tx | (_, TileKind _ _ t) <- Map.toList tm,
             let tx = case t of
                   Door -> "+"
                   Magma -> "*"
                   Open -> "."
                   Rock -> ":"
                   Rubble -> "%"
                   Wall -> "#"
                 ]
  in textList

-- | insertVault at pos
-- TODO Open Hallway
insertVault :: Coord -> TileMap -> TileMap -> TileMap
insertVault (startX, startY) vault tm = let
  -- insert Vault
  vaultList = [ (xy,  tk) | (_, TileKind (x, y) v t) <- Map.toList vault,
                 let tk = TileKind xy v t
                     xy = (startX + x, startY + y) ]
  coordMap = Map.fromList vaultList
  updateMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy coordMap of
                      Just x -> x
                      Nothing -> TileKind xy v t]
  in Map.fromList updateMap

-- | Monster lair
lair :: TileMap
lair = let
  --d = cave 1 10 10
  d = GD.boxDungeon 10 10
  tm = GT.mkTileMap d
  in add (9,9) [Door] tm

-- | uniform grid
mkGrid :: Coord -> Int -> Int -> [Coord]
mkGrid pos maxX maxY = let
  (startX, startY) = pos
  maxXY = if startX+maxX > startY+maxY then startX+maxX else startY+maxY
  in [(y, x) | x <- [startX..maxXY-1], y <- [startY..maxXY-1]]

-- | IO for visualization
showVault :: Int -> TileMap -> IO ()
showVault width tx = do
  let txList = zip [1::Int ..] $ drawVault tx
  forM_ txList $ \(i, j) -> do
    let t = if i `mod` width == 0
          then T.append j (T.pack "\n") else j
    printf "%s" t

-- | 1 x 2
spot :: [Terrain]
spot = replicate 2 Wall

-- | nice little town
town :: TileMap
town = let
  d = GD.boxDungeon 10 10
  tm = GT.mkTileMap d
  a = add (6,4) board $ add (5,4) board tm
  b = add (3,4) board $ add (2,4) board a
  final = add (1,1) [Rock] $ add (8,4) spot b
  in final
