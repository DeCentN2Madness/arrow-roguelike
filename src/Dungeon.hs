{-# LANGUAGE Trustworthy #-}
{-

Dungeon.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Dungeon (
               Terrain(..)
               , Dungeon(..)
               , boxDungeon
               , rogueDungeon
               , getTerrainAt
               ) where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random.Class (MonadRandom, uniformMay)
import Control.Monad.Random (getRandomR, RandomGen, runRandT)
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
  } deriving (Read, Show)

data Orientation = Vertical | Horizontal deriving (Show)

data Room = Room !Int !Int !Int !Int
  deriving (Show)

type Hall = Room

data Terrain
  = Open
  | Wall
  | Rubble
  | StairsDown
  | StarsUp
  deriving (Read, Show, Eq)

-- | boxDungeon builds the dungeon
-- Basic box
boxDungeon :: Int -> Int -> Dungeon
boxDungeon xMax yMax = Dungeon xMax yMax tiles
  where
    tiles =  V.generate (xMax*yMax)
      (\i -> let (y, x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
        then Wall
        else Open)

-- | getTerrainAt
getTerrainAt :: (Int, Int) -> Dungeon -> Terrain
getTerrainAt (x, y) d = let
  width = dungeonWidth d
  height = dungeonHeight d
  index = y * width + x
  in if y < 0 || x < 0 || y >= height || x >= width
    then Wall
    else V.unsafeIndex (dungeonTiles d) index

mkRoom :: (Int,Int) -> (Int,Int) -> Room
mkRoom (x1,y1) (x2,y2) = let
  xlow = min x1 x2
  ylow = min y1 y2
  xhigh = max x1 x2
  yhigh = max y1 y2
  in Room xlow ylow xhigh yhigh

overlapX :: Room -> Room -> Set Int
overlapX (Room r1xl _ r1xh _) (Room r2xl _ r2xh _) =
  S.intersection
    (S.fromAscList [r1xl..r1xh])
    (S.fromAscList [r2xl..r2xh])

overlapY :: Room -> Room -> Set Int
overlapY (Room r1yl _ r1yh _) (Room r2yl _ r2yh _) =
  S.intersection
    (S.fromAscList [r1yl..r1yh])
    (S.fromAscList [r2yl..r2yh])

pickHallways :: MonadRandom m
  => Room
  -> Room
  -> Orientation
  -> m [Hall]
pickHallways r1@(Room r1xl r1yl r1xh r1yh) r2@(Room r2xl r2yl r2xh r2yh ) Vertical = do
  mayX <- uniformMay (overlapX r1 r2)
  case mayX of
    Just x -> pure [mkRoom (x,min r1yl r2yl) (x,max r1yh r2yh)]
    Nothing -> do
      pure []
pickHallways r1@(Room r1xl r1yl r1xh r1yh) r2@(Room r2xl r2yl r2xh r2yh ) Horizontal = do
  mayY <- uniformMay (overlapY r1 r2)
  case mayY of
    Just y -> pure [mkRoom (min r1xl r2xl,y) (max r1xh r2xh,y)]
    Nothing -> do
      pure []

randRoom :: (MonadRandom m) => Int -> Int -> Int -> Int -> m Room
randRoom xlow xhigh ylow yhigh = do
  x1 <- getRandomR (xlow,xhigh)
  x2 <- getRandomR (xlow,xhigh)
  y1 <- getRandomR (ylow,yhigh)
  y2 <- getRandomR (ylow,yhigh)
  pure $ mkRoom (x1,y1) (x2,y2)

-- | rogueDungeon build the Dungeon
rogueDungeon :: RandomGen g => Int -> Int -> g -> (Dungeon, g)
rogueDungeon width height g = let
  tileCount = width*height
  secWidth = width `div` 3
  secHeight = height `div` 3
  (tileVector, gFinal) = runST $ flip runRandT g $ do
    vec <- VM.replicate tileCount Wall
    -- pick the rooms
    rooms <- sequence [
            randRoom   1 (secWidth-1) 1 (secHeight-1)
            , randRoom (secWidth+1) (2*secWidth-1) 1 (secHeight-1)
            , randRoom (2*secWidth+1) (width-2) 1 (secHeight-1)
            , randRoom 1 (secWidth-1) (secHeight+1) (2*secHeight-1)
            , randRoom (secWidth+1) (2*secWidth-1) (secHeight+1)  (2*secHeight-1)
            , randRoom (2*secWidth+1) (width-2) (secHeight+1) (2*secHeight-1)
            , randRoom 1 (secWidth-1) (2*secHeight+1) (height-2)
            , randRoom (secWidth+1) (2*secWidth-1) (2*secHeight+1) (height-2)
            , randRoom (2*secWidth+1) (width-2) (2*secHeight+1) (height-2)
            ]
    -- draw the sectors
    forM_ rooms $ \r -> setBox width vec r Open
    -- connections
  {-
    forM_ [1..12] $ \borderIndex -> do
      let (sec1Targ, sec2Targ, isVert) = case borderIndex of
            1 -> (1,2,False)
            2 -> (2,3,False)
            3 -> (4,5,False)
            4 -> (5,6,False)
            5 -> (7,8,False)
            6 -> (8,9,False)
            7 -> (1,4,True)
            8 -> (4,7,True)
            9 -> (2,5,True)
            10 -> (5,8,True)
            11 -> (3,6,True)
            12 -> (6,9,True)
       sec1 = sectors !! (sec1Targ-1)
       sec2 = sectors !! (sec2Targ-1)
    -- line up rooms with hall
    halls <- pickHallways sec1 sec2 isVert
    forM_ halls $ \(x1,y1,x2,y2) -> setBox width vec (x1,y1) (x2,y2) Open
-}
    -- TODO connect the sectors
    V.unsafeFreeze vec
  in (Dungeon width height tileVector, gFinal)

-- | setBox
setBox :: PrimMonad m
  => Int
  -> VM.MVector (PrimState m) a
  -> Room
  -> a
  -> m ()
setBox width vec (Room x1 y1 x2 y2) tile = do
  let xmin = min x1 x2
      xmax = max x1 x2
      ymin = min y1 y2
      ymax = max y1 y2
      targets = do
        y <- [ymin .. ymax]
        x <- [xmin .. xmax]
        pure $ width*y+x
  mapM_ (\i -> VM.write vec i tile) targets
