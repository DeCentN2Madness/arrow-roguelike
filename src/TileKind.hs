{-

TileKind.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module TileKind(TileKind(..)
               , zeroTK) where

import Dungeon (Terrain(..))

type Coord = (Int, Int)

data TileKind = TileKind
  { location :: Coord
  , visible :: Bool
  , tKind :: Terrain
  } deriving (Show)

-- | zeroG useful for filter
zeroTK :: TileKind
zeroTK = TileKind (0,0) False Zero
