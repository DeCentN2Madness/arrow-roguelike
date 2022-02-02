{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..)
                        , EntityKind(..)
                        , mkEntity
                        , Properties) where

import Data.Map (Map)
import qualified Data.Map as Map
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
  { coord        :: Coord
  , block        :: Bool
  , eKind        :: Entity
  , prop         :: Properties
  , hitPoint     :: Int
  } deriving (Show)

-- | defaultProp
defaultProp :: Int -> Properties
defaultProp s = let
  std          = Map.toList $ mkProp "Player" "@"
  strength     = DS.d6 s + DS.d6 s + DS.d6 s
  dexterity    = DS.d6 s + DS.d6 s + DS.d6 s
  constitution = DS.d6 s + DS.d6 s + DS.d6 s
  intelligence = DS.d6 s + DS.d6 s + DS.d6 s
  wisdom       = DS.d6 s + DS.d6 s + DS.d6 s
  stats = [ ("str", show strength)
          , ("int", show intelligence)
          , ("dex", show dexterity)
          , ("con", show constitution)
          , ("wis", show wisdom)
          ] ++ std
  in Map.fromList stats

-- | mouseProp
-- 2d6 stats, 3d6 dex
mouseProp :: Int -> Properties
mouseProp s = let
  std          = Map.toList $ mkProp "Mouse" "r"
  strength     = DS.d6 s + DS.d6 s
  dexterity    = DS.d6 s + DS.d6 s + DS.d6 s
  constitution = DS.d6 s + DS.d6 s
  intelligence = 2 :: Int
  wisdom       = 10 :: Int
  stats = [ ("str", show strength)
          , ("int", show intelligence)
          , ("dex", show dexterity)
          , ("con", show constitution)
          , ("wis", show wisdom)
          ] ++ std
  in Map.fromList stats

-- | mkProp
mkProp :: String -> String -> Properties
mkProp x y = Map.fromList [("Name", x), ("Desc", y)]

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy     = EntityKind xy True Actor (defaultProp 1) 10
mkEntity Coin xy      = EntityKind xy False Coin (mkProp "Coin" "$") 0
mkEntity Corpse xy    = EntityKind xy False Corpse (mkProp "Corpse" "%") 0
mkEntity Item xy      = EntityKind xy False Item (mkProp "Item" "[") 0
mkEntity Mouse xy     = EntityKind xy True Mouse (mouseProp 2) 7
mkEntity Mushroom xy  = EntityKind xy False Mushroom  (mkProp "Mushroom" ",") 0
mkEntity Potion xy    = EntityKind xy False Potion (mkProp "Potion" "!") 0
mkEntity StairDown xy = EntityKind xy False StairDown (mkProp "Stair" ">") 0
mkEntity StairUp xy   = EntityKind xy False StairUp (mkProp "Stair" "<") 0
mkEntity Trap xy      = EntityKind xy False Trap (mkProp "Trap" "^") 0
mkEntity Unknown xy   = EntityKind xy False Unknown (mkProp "Unknown" "~") 0
