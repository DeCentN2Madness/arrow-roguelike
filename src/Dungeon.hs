{-

Dungeon.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Dungeon where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
  , visibleTiles :: Vector Bool
  } deriving (Read, Show)

data Terrain
  = Open
  | Wall
  | Rubble
  | StairsDown
  | StarsUp
  deriving (Read, Show, Eq)

-- | getTerrainAt
getTerrainAt :: (Int, Int) -> Dungeon -> Terrain
getTerrainAt (x, y) d = let
  width = dungeonWidth d
  height = dungeonHeight d
  index = y * width + x
  in if y < 0 || x < 0 || y >= height || x >= width
    then Wall
    else V.unsafeIndex (dungeonTiles d) index

-- | mkDungeon build the dungeon
mkDungeon :: Int -> Int -> Dungeon
mkDungeon xMax yMax = Dungeon xMax yMax tiles visible
  where
    tiles =  V.generate (xMax*yMax)
      (\i -> let (y, x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
        then Wall
        else Open)
    visible = V.generate (xMax*yMax)
      (\i -> let (y,x ) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
        then True
        else False)
