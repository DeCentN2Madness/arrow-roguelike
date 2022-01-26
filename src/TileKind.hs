{-

TileKind.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module TileKind(TileKind(..), zeroTK) where

import Dungeon (Terrain(..))

type Coord = (Int, Int)

data TileKind = TileKind Coord Bool Terrain deriving (Read, Show, Eq)

-- | zeroG useful for filter
zeroTK :: TileKind
zeroTK = TileKind (0,0) False ZeroT
