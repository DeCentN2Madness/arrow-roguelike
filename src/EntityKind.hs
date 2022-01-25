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

mkEntity :: Entity -> EntityKind
mkEntity Actor = EntityKind { coord = (0,0), eKind = Actor, desc = "the Hero", name = "Player", block = True }
mkEntity Bang = EntityKind { coord = (0,0), eKind = Bang, desc = "!", name = "!", block = False }
mkEntity Corpse = EntityKind { coord = (0,0), eKind = Corpse, desc = "%", name = "%", block = False }
mkEntity Item = EntityKind { coord = (0,0), eKind = Item, desc = "[", name = "Item", block = False }
mkEntity Mouse = EntityKind { coord = (0,0), eKind = Mouse, desc = "the Mouse", name = "r", block = True }
mkEntity Mushroom = EntityKind { coord = (0,0), eKind = Mushroom, desc = ",", name = ",", block = False }
mkEntity StairDown = EntityKind { coord = (0,0), eKind = StairDown, desc = ">", name = ">", block = False }
mkEntity StairUp = EntityKind { coord = (0,0), eKind = StairUp, desc = "<", name = "<", block = False }
mkEntity Trap = EntityKind { coord = (0,0), eKind = Trap, desc = "^", name = "Trap", block = False }
mkEntity ZeroE = zeroEK


-- | zeroE useful for filter
zeroEK :: EntityKind
zeroEK = EntityKind (0,0) False ZeroE "" ""
