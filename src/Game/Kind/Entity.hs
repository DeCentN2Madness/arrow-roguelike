{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..), EntityKind(..), mkEntity) where

import Control.Monad.Random (StdGen)
import Data.Map (Map)
import qualified Data.Map as Map

type Coord = (Int, Int)
type Properties = Map String String

data Entity
  = Actor
  | Bang
  | Corpse
  | Item
  | Mouse
  | Mushroom
  | StairDown
  | StairUp
  | Trap
  deriving (Show, Eq, Ord)

data EntityKind = EntityKind
  { coord :: Coord
  , block :: Bool
  , eKind :: Entity
  , prop :: Properties
  , _gameGen :: StdGen
  } deriving (Show)

-- | defaultProp
-- this will do more
defaultProp :: Properties
defaultProp = Map.fromList [("Name", "Player"), ("Desc", "@")]

-- | mkEntity
--mkEntity :: RandomGen g => Entity -> Coord -> g -> (EntityKind, g)
mkEntity :: Entity -> Coord -> StdGen -> EntityKind
mkEntity Actor xy g     = EntityKind xy True Actor defaultProp g
mkEntity Bang xy g      = EntityKind xy False Bang  defaultProp g
mkEntity Corpse xy g    = EntityKind xy False Corpse defaultProp g
mkEntity Item xy g      = EntityKind xy False Item defaultProp g
mkEntity Mouse xy g     = EntityKind xy True Mouse defaultProp g
mkEntity Mushroom xy g  = EntityKind xy False Mushroom defaultProp g
mkEntity StairDown xy g = EntityKind xy False StairDown defaultProp g
mkEntity StairUp xy g   = EntityKind xy False StairUp defaultProp g
mkEntity Trap xy g      = EntityKind xy False Trap defaultProp g
