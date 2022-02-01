{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..), EntityKind(..), mkEntity) where

import Control.Monad.Random (StdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import Game.DiceSet (Die(..))
import qualified Game.DiceSet as DS

type Coord = (Int, Int)
type Properties = Map String String

data Entity
  = Actor
  | Coin
  | Corpse
  | Item
  | Mouse
  | Mushroom
  | Potion
  | StairDown
  | StairUp
  | Trap
  | Unknown
  deriving (Show, Eq, Ord)

data EntityKind = EntityKind
  { coord :: !Coord
  , block :: !Bool
  , eKind :: !Entity
  , prop :: !Properties
  , _gameGen :: !StdGen
  } deriving (Show)

abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | defaultProp
defaultProp :: StdGen -> Properties
defaultProp g = let
  std          = Map.toList $ mkProp "Player" "@"
  strength     = DS.roll 10 9 3 6 g
  dexterity    = DS.roll 20 9 3 6 g
  constitution = DS.roll 30 9 3 6 g
  intelligence = DS.roll 40 9 3 6 g
  wisdom       = DS.roll 50 9 3 6 g
  attack       = abilityMod strength
  defense      = abilityMod dexterity
  hitPoints    = abilityMod constitution + 10
  stats = [ ("str", show strength)
          , ("int", show intelligence)
          , ("dex", show dexterity)
          , ("con", show constitution)
          , ("wis", show wisdom)
          , ("ar",  show attack)
          , ("dr",  show defense)
          , ("hp",  show hitPoints)
          ] ++ std
  in Map.fromList stats

-- | mouseProp
-- 2d6 stats, 3d6 dex
mouseProp :: StdGen -> Properties
mouseProp g = let
  std          = Map.toList $ mkProp "Mouse" "r"
  strength     = DS.rollMod D6 0 g + DS.rollMod D6 0 g
  dexterity    = DS.rollMod D6 0 g + DS.rollMod D6 0 g + DS.rollMod D6 0 g
  constitution = DS.rollMod D6 0 g + DS.rollMod D6 0 g
  intelligence = 2 :: Int
  wisdom       = 10 :: Int
  attack       = abilityMod strength
  defense      = abilityMod dexterity
  hitPoints    = abilityMod constitution +
    DS.rollMod D6 0 g +
    DS.rollMod D6 0 g
  stats = [ ("str", show strength)
          , ("int", show intelligence)
          , ("dex", show dexterity)
          , ("con", show constitution)
          , ("wis", show wisdom)
          , ("ar",  show attack)
          , ("dr",  show defense)
          , ("hp",  show hitPoints)
          ] ++ std
  in Map.fromList stats

-- | mkProp
mkProp :: String -> String -> Properties
mkProp x y = Map.fromList [("Name", x), ("Desc", y)]

-- | mkEntity
mkEntity :: Entity -> Coord -> StdGen -> EntityKind
mkEntity Actor xy g     = EntityKind xy True Actor (defaultProp g) g
mkEntity Coin xy g      = EntityKind xy False Coin (mkProp "Coin" "$") g
mkEntity Corpse xy g    = EntityKind xy False Corpse (mkProp "Corpse" "%") g
mkEntity Item xy g      = EntityKind xy False Item (mkProp "Item" "[") g
mkEntity Mouse xy g     = EntityKind xy True Mouse (mouseProp g) g
mkEntity Mushroom xy g  = EntityKind xy False Mushroom  (mkProp "Mushroom" ",") g
mkEntity Potion xy g    = EntityKind xy False Potion (mkProp "Potion" "!") g
mkEntity StairDown xy g = EntityKind xy False StairDown (mkProp "Stair" ">") g
mkEntity StairUp xy g   = EntityKind xy False StairUp (mkProp "Stair" "<") g
mkEntity Trap xy g      = EntityKind xy False Trap (mkProp "Trap" "^") g
mkEntity Unknown xy g   = EntityKind xy False Unknown (mkProp "Unknown" "~") g
