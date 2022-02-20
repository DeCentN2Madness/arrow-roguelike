{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Engine.Arrow.Data.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Data where

import Control.Monad.Random (StdGen)
import Data.Aeson
import GHC.Generics
import Game.Dungeon (rogueDungeon)
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Journal (TextMap)
import qualified Game.Journal as GJ
import Game.Tile (TileMap)
import qualified Game.Tile as GT
--import qualified Game.Vault as GV

type Coord = (Int, Int)

data Direction
  = Help
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  | B
  | C
  | E
  | G
  | H
  | I
  | J
  | K
  | L
  | N
  | R
  | Q
  | U
  | Y
  deriving (Eq)

data Intent
  = Action Direction
  | Idle
  | Quit

data World = World
  { -- GameWorld
  tick       :: !Int
  , gameT    :: !TileMap
  , entityT  :: !EntityMap
  , assetT   :: !EntityMap
  , journalT :: !TextMap
  , fovT     :: ![Coord]
  -- XY for Screen
  , gridXY   :: !Coord
  , cameraXY :: !(Double, Double)
  , screenXY :: !(Double, Double)
  , scaleXY  :: !(Double, Double)
  -- GameStates
  , dirty    :: !Bool
  , starting :: !Bool
  , exiting  :: !Bool
  } deriving (Show, Generic)

instance FromJSON World
instance ToJSON World where
  toJSON World{..} = object [
    "tick"       .= tick
    , "gameT"    .= gameT
    , "entityT"  .= entityT
    , "assetT"   .= assetT
    , "journalT" .= journalT
    , "fovT"     .= fovT
    , "gridXY"   .= gridXY
    , "cameraXY" .= cameraXY
    , "screenXY" .= screenXY
    , "scaleXY"  .= scaleXY
    , "dirty"    .= True
    , "starting" .= True
    , "exiting"  .= False
    ]

-- | mkWorld build the World
mkWorld :: StdGen -> Coord -> Int -> Int -> World
mkWorld gen (width, height) xMax yMax = let
  (d, _) = rogueDungeon xMax yMax gen
  am = GE.mkAssetMap []
  em = GE.mkEntityMap tm am
  jm = GJ.mkTextMap
  --tm = GV.insertVault (1,1) GV.lair $ GT.mkTileMap d
  tm = GT.mkTileMap d
  sx = 32.0 -- scaleXY based on tiles
  sy = 32.0
  in World { tick     = 1
           , gameT    = tm
           , entityT  = em
           , assetT   = am
           , journalT = GJ.updateJournal ["Catch the Mice...", "Welcome to Arrow..."] jm
           , fovT     = []
           , gridXY   = (xMax, yMax)
           , cameraXY = (0.0, 0.0)
           , screenXY = (fromIntegral width, fromIntegral height)
           , scaleXY  = (sx, sy)
           , dirty    = True
           , starting = True
           , exiting  = False
           }
