{-

Dungeon.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Dungeon where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random (getRandomR, RandomGen, runRandT)
import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
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
mkDungeon xMax yMax = Dungeon xMax yMax tiles
  where
    tiles =  V.generate (xMax*yMax)
      (\i -> let (y, x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
        then Wall
        else Open)

-- | rogueDungeon build the Dungeon
rogueDungeon :: RandomGen g => Int -> Int -> g -> (Dungeon, g)
rogueDungeon width height g = let
  (tileVector, gFinal) = runST $ flip runRandT g $ do
    let count = width*height
    vec <- VM.replicate count Open
    x1 <- getRandomR (1,20)
    x2 <- getRandomR (1,20)
    y1 <- getRandomR (1,20)
    y2 <- getRandomR (1,20)
    setBox width vec (x1,y1) (x2,y2) Wall
    V.unsafeFreeze vec
  in (Dungeon width height tileVector, gFinal)

-- | setBox
setBox :: PrimMonad m
  => Int
  -> VM.MVector (PrimState m) a
  -> (Int, Int)
  -> (Int, Int)
  -> a
  -> m ()
setBox width vec (x1,y1) (x2,y2) tile = do
  let xmin = min x1 x2
      xmax = max x1 x2
      ymin = min y1 y2
      ymax = max y1 y2
      targets = do
        y <- [ymin .. ymax]
        x <- [xmin .. xmax]
        pure $ width*y+x
  mapM_ (\i -> VM.write vec i tile) targets
