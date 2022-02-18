{-# LANGUAGE OverloadedStrings #-}
{-

Game.Vault.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Vault (lair
                   , showVault
                   , town) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Dungeon (boxDungeon)
import Game.Tile (TileMap)
import qualified Game.Tile as GT
import Game.Kind.Tile (Terrain(..), TileKind (..))
import Text.Printf

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
board = toTerrain (map T.pack [ "#", "#", "#", "#" ])

-- | 1 x 1
dot :: Terrain -> [Terrain]
dot t = [t]

-- | TileMap to Text
drawVault :: TileMap -> [Text]
drawVault tm = let
  textList = [ v | (_, TileKind _ _ t) <- Map.toList tm,
             let v = case t of
                   Wall -> "#"
                   Rock -> ":"
                   Rubble -> "%"
                   Magma -> "*"
                   Open -> "." ]
  in textList

-- | Monster lair
lair :: TileMap
lair = let
  d = boxDungeon 10 10
  tm = GT.mkTileMap d
  a = add (1,1) (dot Rubble) $ add (1,8) (dot Rubble) $ add (1,4) spot tm
  b = add (3,4) (dot Rubble) $ add (4,5) (dot Rubble) $ add (5,4) (dot Rubble) a
  final = add (8,1) (dot Rubble) $ add (8,8) (dot Rubble) $ add (8,4) spot b
  in final

-- | uniform grid
mkGrid :: Coord -> Int -> Int -> [Coord]
mkGrid pos maxX maxY = let
  (startX, startY) = pos
  maxXY = if startX+maxX > startY+maxY then startX+maxX else startY+maxY
  in [(y, x) | x <- [startX..maxXY-1], y <- [startY..maxXY-1]]

-- | IO for visualization
showVault :: TileMap -> IO ()
showVault tx = do
  let txList = zip [1::Int ..] $ drawVault tx
  forM_ txList $ \(i, j) -> do
    let t = if i `mod` 10 == 0
          then T.append j (T.pack "\n") else j
    printf "%s" t

-- | Text to Terrain
toTerrain :: [Text] -> [Terrain]
toTerrain tx = let
  terrainList = [ v | t <- tx,
                  let v = case t of
                        "#" -> Wall
                        ":" -> Rock
                        "%" -> Rubble
                        "*" -> Magma
                        "." -> Open
                        _   -> Wall ]
  in terrainList

-- | nice little town
town :: TileMap
town = let
  d = boxDungeon 10 10
  tm = GT.mkTileMap d
  a = add (6,4) board $ add (5,4) board tm
  b = add (3,4) board $ add (2,4) board a
  final = add (1,1) (dot Rock) $ add (8,4) spot b
  in final

-- | 1 x 2
spot :: [Terrain]
spot = toTerrain (map T.pack [ "#", "#" ])
