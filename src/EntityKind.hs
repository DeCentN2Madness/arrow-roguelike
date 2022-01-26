{-

EntityKind.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module EntityKind(Entity(..)
                 , EntityKind(..)
                 , mkEntity
                 , zeroEK) where

type Coord = (Int, Int)

data Entity
  = Actor
  | Bang
  | Corpse
  | Item
  | Mouse
  | Mushroom
  | Trap
  | StairDown
  | StairUp
  | ZeroE
  deriving (Read, Show, Eq)

data EntityKind = EntityKind
  { coord :: Coord
  , block :: Bool
  , eKind :: Entity
  , desc :: String
  , name :: String
  } deriving (Show)

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy     = EntityKind xy True Actor "the Hero" "Player"
mkEntity Bang xy      = EntityKind xy False Bang  "the potion" "!"
mkEntity Corpse xy    = EntityKind xy False Corpse "the corpse" "%"
mkEntity Item xy      = EntityKind xy False Item "the Item" "["
mkEntity Mouse xy     = EntityKind xy True Mouse "the Mouse" "r"
mkEntity Mushroom xy  = EntityKind xy False Mushroom "the Mushroom" ","
mkEntity StairDown xy = EntityKind xy False StairDown "stairDown" ">"
mkEntity StairUp xy   = EntityKind xy False StairUp "stairUp" "<"
mkEntity Trap xy      = EntityKind xy False Trap "the Trap" "^"
mkEntity ZeroE _      = zeroEK

-- | zeroE useful for filter
zeroEK :: EntityKind
zeroEK = EntityKind (0,0) False ZeroE "" ""
