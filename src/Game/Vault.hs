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
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import qualified Engine.Arrow.Compass as EAC
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
doorList tm = nub $ [ xy | (_, TileKind pos _ t) <- Map.toList tm,
                let xy = if t == Door then pos else (0,0) ]

-- | openList
openList :: TileMap -> [Coord]
openList tm = [ xy | (_, TileKind pos _ t) <- Map.toList tm,
                let xy = if t == Open then pos else (0,0) ]

-- | need pattern match
eastOpen :: Coord -> TileMap -> [Coord]
eastOpen (i, j) tm = [ xy | (x, y) <- openList tm,
             let xy = if j == y && x > i then (x, y) else (0,0) ]

westOpen :: Coord -> TileMap -> [Coord]
westOpen (i, j) tm = [ xy | (x, y) <- openList tm,
             let xy = if j == y && x < i then (x, y) else (0,0) ]

northOpen :: Coord -> TileMap -> [Coord]
northOpen (i, j) tm = [ xy | (x, y) <- openList tm,
             let xy = if x == i && y > j then (x, y) else (0,0) ]

southOpen :: Coord -> TileMap -> [Coord]
southOpen (i, j) tm = [ xy | (x, y) <- openList tm,
             let xy = if x == i && y > j then (x, y) else (0,0) ]

-- | door facing south
southHall :: Coord -> TileMap -> [Coord]
southHall (x, y) tm = let
  (i, j) = (x, y+1)
  east = eastOpen (i, j) tm
  south = southOpen (i, j) tm
  west = westOpen (i, j) tm
  in nub $ concat [east, west, south]

-- | door facing north
northHall :: Coord -> TileMap -> [Coord]
northHall (x, y) tm = let
  (i, j) = (x, y-1)
  north = northOpen (i, j) tm
  east = eastOpen (i, j) tm
  west = westOpen (i, j) tm
  in nub $ concat [north, east, west]

-- | door facing east
eastHall :: Coord -> TileMap -> [Coord]
eastHall (x, y) tm = let
  (i, j) = (x-1, y)
  north = northOpen (i, j) tm
  east = eastOpen (i, j) tm
  south = southOpen (i, j) tm
  in nub $ concat [north, east, south]

-- | door facing west
westHall :: Coord -> TileMap -> [Coord]
westHall (x, y) tm = let
  (i, j) = (x+1, y)
  north = northOpen (i, j) tm
  south = southOpen (i, j) tm
  west = westOpen (i, j) tm
  in nub $ concat [north, west, south]


-- | insertVault at pos
-- Game.Vault.showVault $ Game.Vault.insertVault (20,10) l c
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
  -- pickHall by door location
  --south
  --doors  = tail $ doorList $ Map.fromList updateMap
  --theDoor = head doors
  --southPassage = head $ tail $ southHall theDoor $ Map.fromList updateMap
  --newDoor = (fst theDoor, snd theDoor + 1)
  --oList = replicate (EAC.chessDist newDoor southPassage) Open
  --passageMap = add newDoor oList $ Map.fromList updateMap
  in Map.fromList updateMap

-- | Monster lair
lair :: TileMap
lair = let
  --d = cave 1 10 10
  d = GD.boxDungeon 10 10
  tm = GT.mkTileMap d
  in add (9,5) [Door] tm

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
      doors  = tail $ doorList tm
      theDoor = head doors
      west = westHall theDoor tm
      north = northHall theDoor tm
      east = eastHall theDoor tm
      south = southHall theDoor tm

  forM_ txList $ \((i,_), t) -> do
    let vt = if i == 0
          then T.append (T.pack "\n") t
          else t
    printf "%s" vt
  printf "\n"
  printf "door = %s\n" (show doors)
  printf "north = %s\n" (show north)
  printf "east = %s\n" (show east)
  printf "west = %s\n" (show west)
  printf "south = %s\n" (show south)
  --printf "above = %s\n" (show $ aboveDoor theDoor tx)
  --printf "right = %s\n" (show $ rightDoor theDoor tx)
  --printf "left = %s\n"  (show $ leftDoor  theDoor tx)
  --printf "below = %s\n" (show $ belowDoor theDoor tx)

-- | 1 x 2
spot :: [Terrain]
spot = replicate 2 Wall

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
  tm = GT.mkTileMap d
  a = add (6,4) board $ add (5,4) board tm
  b = add (3,4) board $ add (2,4) board a
  final = add (1,1) [Rock] $ add (8,4) spot b
  in final
