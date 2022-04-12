{-# LANGUAGE DeriveGeneric #-}
{-

Game.Kind.Tile

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Tile (addLit
                      , addVisual
                      , Dungeon(..)
                      , Terrain(..)
                      , TileKind(..)
                      , VisualKind(..)) where

import Data.Aeson
import GHC.Generics
import Game.Kind.Dungeon (Dungeon(..), Terrain(..))
import Game.Kind.Visual

type Coord = (Int, Int)
data TileKind
  = TileKind
  !Coord
  !Bool
  !Terrain
  !VisualKind -- Visual
  !VisualKind -- Visual Lit
  deriving (Show, Eq, Generic)

instance FromJSON TileKind
instance ToJSON TileKind

addLit :: Terrain -> VisualKind
addLit t
  | t == Door   = VDoor
  | t == Magma  = VLMagma
  | t == Open   = VLOpen
  | t == Rock   = VLRock
  | t == Rubble = VLRubble
  | otherwise   = VLWall

addVisual :: Terrain -> VisualKind
addVisual t
  | t == Door   = VDoor
  | t == Magma  = VMagma
  | t == Open   = VOpen
  | t == Rock   = VRock
  | t == Rubble = VRubble
  | otherwise   = VWall
