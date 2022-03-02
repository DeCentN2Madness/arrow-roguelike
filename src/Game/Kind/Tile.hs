{-# LANGUAGE DeriveGeneric #-}
{-

Game.Kind.Tile

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Tile (Dungeon(..)
                      , Terrain(..)
                      , TileKind(..)) where

import Data.Aeson
import GHC.Generics
import Game.Kind.Dungeon

type Coord = (Int, Int)
data TileKind = TileKind !Coord !Bool !Terrain deriving (Show, Eq, Generic)

instance FromJSON TileKind
instance ToJSON TileKind
