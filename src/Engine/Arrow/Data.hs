{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Engine.Arrow.Data.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Data where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Journal (TextMap)
import qualified Game.Journal as GJ
import Game.Tile (TileMap)
import qualified Game.Vault as GV

type Coord = (Int, Int)
type Seed = Int
type Depth = Int

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
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  deriving (Eq)

data GameState
  = GameStart
  | GameRun
  | GameStop
  | GameAnimation
  | GameDialog
  | GameDrop
  | GameEquipment
  | GameInventory
  | GameStore
  deriving (Eq, Show, Generic)

instance FromJSON GameState
instance ToJSON GameState

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
  -- GameState
  , dirty     :: !Bool
  , gameState :: !GameState
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
    , "gameState" .= GameStart
    ]

-- | mkWorld build the World
mkWorld :: Seed -> Coord -> Depth -> Int -> Int -> World
mkWorld seed (width, height) depth xMax yMax = let
  assetMap   = GE.mkAssetMap
  entityMap  = GE.mkEntityMap depth gameMap assetMap
  journalMap = GJ.mkTextMap
  gameMap    = GV.mkGameMap seed depth xMax yMax
  sx = 32.0 -- scaleXY based on tiles
  sy = 32.0
  in World { tick     = seed
           , gameT    = gameMap
           , entityT  = entityMap
           , assetT   = assetMap
           , journalT = GJ.updateJournal startJournal journalMap
           , fovT     = []
           , gridXY   = (xMax, yMax)
           , cameraXY = (0.0, 0.0)
           , screenXY = (fromIntegral width, fromIntegral height)
           , scaleXY  = (sx, sy)
           , dirty    = True
           , gameState = GameStart
           }

startJournal :: [Text]
startJournal = [ "Beware of Trolls..."
               , "Catch the Mice..."
               , "Welcome to Arrow...." ]
