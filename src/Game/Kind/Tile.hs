{-

Game.Kind.Tile

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Tile (TileKind(..), zeroTK) where

import Game.Dungeon (Terrain(..))

type Coord = (Int, Int)

data TileKind = TileKind Coord Bool Terrain deriving (Show, Eq)

-- | zeroG useful for filter
zeroTK :: TileKind
zeroTK = TileKind (0,0) False ZeroT
