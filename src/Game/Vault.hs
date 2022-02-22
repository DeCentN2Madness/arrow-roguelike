{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (cave
                  , insertVault
                  , showVault
                  , testVault
                  , town) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.Random (mkStdGen)
import Data.List
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
  terrainMap = Map.fromList $ zip ixList $ [ tk | (xy, t) <- terrainList,
               let tk = TileKind xy True t ]
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

-- | doorList
doorList :: TileMap -> [Coord]
doorList tm = nub $ filter (/=(0,0)) $
  [ xy | (_, TileKind pos _ t) <- Map.toList tm,
                let xy = if t == Door then pos else (0,0) ]

insertHall :: Coord -> Int -> TileMap -> TileMap
insertHall (x1, y1) dist tm = let
  ts = replicate dist Open
  coordList = mkGrid (y1, x1) (length ts) (length ts)
  terrainList = zip coordList ts
  hallMap =  Map.fromList terrainList
  finalMap = [ (ix, tk) | (ix, TileKind xy v t) <- Map.toList tm,
                let tk = case Map.lookup xy hallMap of
                      Just x -> TileKind xy False x
                      Nothing -> TileKind xy v t]
  in Map.fromList finalMap

-- | insertVault at pos
--   Game.Vault.insertVault (1,1) town dungeon
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
  hallMap = insertHall (6,11) 10 updateMap
  in hallMap

-- | uniform grid
mkGrid :: Coord -> Int -> Int -> [Coord]
mkGrid pos maxX maxY = let
  (startX, startY) = pos
  maxXY = if startX+maxX > startY+maxY then startX+maxX else startY+maxY
  in [(y, x) | x <- [startX..maxXY-1], y <- [startY..maxXY-1]]

-- | IO for visualization
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
  a = add (6,4) board  $ add (5,4) board t
  b = add (3,4) board  $ add (2,4) board a
  c = add (9,5) [Door] b
  in c

testVault :: IO ()
testVault = do
  let d = cave 1 80 40
      l = town
  showVault $ insertVault (1,1) l d
