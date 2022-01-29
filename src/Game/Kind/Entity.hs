{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..), EntityKind(..), mkEntity) where

import Control.Monad.Random (StdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Game.DiceSet as DS

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
defaultProp :: StdGen -> Properties
defaultProp g = let
  std          = Map.toList $ mkProp "Player" "@"
  strength     = DS.bigRoll 1000 g
  intelligence = DS.bigRoll 2000 g
  dexterity    = DS.bigRoll 3000 g
  wisdom       = DS.bigRoll 4000 g
  constitution = DS.bigRoll 5000 g
  stats = [ ("Str", show strength)
          , ("Int", show intelligence)
          , ("Dex", show dexterity)
          , ("Wis", show wisdom)
          , ("Con", show constitution)] ++ std
  in Map.fromList stats

-- | mkProp
mkProp :: String -> String -> Properties
mkProp x y = Map.fromList [("Name", x), ("Desc", y)]

-- | mkEntity
mkEntity :: Entity -> Coord -> StdGen -> EntityKind
mkEntity Actor xy g     = EntityKind xy True Actor (defaultProp g) g
mkEntity Bang xy g      = EntityKind xy False Bang  (mkProp "Bang" "!") g
mkEntity Corpse xy g    = EntityKind xy False Corpse (mkProp "Corpse" "%") g
mkEntity Item xy g      = EntityKind xy False Item (mkProp "Item" "[") g
mkEntity Mouse xy g     = EntityKind xy True Mouse (mkProp "Mouse" "r") g
mkEntity Mushroom xy g  = EntityKind xy False Mushroom (mkProp "Mushroom" ",") g
mkEntity StairDown xy g = EntityKind xy False StairDown (mkProp "Stair" ">") g
mkEntity StairUp xy g   = EntityKind xy False StairUp (mkProp "Stair" "<") g
mkEntity Trap xy g      = EntityKind xy False Trap (mkProp "Trap" "^") g
