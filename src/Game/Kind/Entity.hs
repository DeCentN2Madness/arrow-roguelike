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

-- | fighterProp
fighterProp :: Properties
fighterProp = let
  std          = Map.toList $ mkProp "Player" "@"
  strength     = 15 :: Int
  dexterity    = 14 :: Int
  constitution = 13 :: Int
  intelligence = 12 :: Int
  wisdom       = 10 :: Int
  stats = [ ("str", show strength)
          , ("int", show intelligence)
          , ("dex", show dexterity)
          , ("con", show constitution)
          , ("wis", show wisdom)
          ] ++ std
  in Map.fromList stats

-- | mouseProp
mouseProp :: Properties
mouseProp = let
  std          = Map.toList $ mkProp "Mouse" "r"
  strength     = 7  :: Int
  dexterity    = 15 :: Int
  constitution = 11 :: Int
  intelligence = 2  :: Int
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
mkEntity Actor xy     = EntityKind xy True Actor fighterProp 10
mkEntity Coin xy      = EntityKind xy False Coin (mkProp "Coin" "$") 1
mkEntity Corpse xy    = EntityKind xy False Corpse (mkProp "Corpse" "%") 1
mkEntity Item xy      = EntityKind xy False Item (mkProp "Item" "[") 1
mkEntity Mouse xy     = EntityKind xy True Mouse mouseProp 7
mkEntity Mushroom xy  = EntityKind xy False Mushroom  (mkProp "Mushroom" ",") 1
mkEntity Potion xy    = EntityKind xy False Potion (mkProp "Potion" "!") 1
mkEntity StairDown xy = EntityKind xy False StairDown (mkProp "Stair" ">") 1
mkEntity StairUp xy   = EntityKind xy False StairUp (mkProp "Stair" "<") 1
mkEntity Trap xy      = EntityKind xy False Trap (mkProp "Trap" "^") 1
mkEntity Unknown xy   = EntityKind xy False Unknown (mkProp "Unknown" "~") 1
