{-

EntityKind.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module EntityKind(Entity(..)
                 , EntityKind(..)
                 , zeroEK) where

type Coord = (Int, Int)

data Entity
  = Actor
  | Item
  | Player
  | Trap
  | Stair
  deriving (Read, Show, Eq)

data EntityKind = EntityKind
  { coord :: Coord
  , block :: Bool
  , eKind :: Entity
  , desc :: String
  , name :: String
  } deriving (Show)

-- | zeroE useful for filter
zeroEK :: EntityKind
zeroEK = EntityKind (0,0) False Actor "" ""
