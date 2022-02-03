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
  , kind         :: Entity
  , prop         :: Properties
  , hitPoint     :: Int
  , moveT         :: [Coord] -- where can the Entity move?
  } deriving (Show)

defaultEK :: EntityKind
defaultEK = EntityKind (0,0) False Unknown (mkProp "Unknown" "~") 0 []

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
mkEntity Actor xy     = let
  e = defaultEK
  in e { coord=xy, block=True, kind=Actor, prop=fighterProp, hitPoint=10 }
mkEntity Coin xy      = let
  e = defaultEK
  in e { coord=xy, kind=Coin, prop=mkProp "Coin" "$" }
mkEntity Corpse xy    = let
  e = defaultEK
  in e { coord=xy, kind=Corpse, prop=mkProp "Corpse" "%" }
mkEntity Item xy      = let
  e = defaultEK
  in e { coord=xy, kind=Item, prop=mkProp "Item" "[" }
mkEntity Mouse xy     = let
  e = defaultEK
  in e { coord=xy, block=True, kind=Mouse, prop=mouseProp, hitPoint=7 }
mkEntity Mushroom xy  = let
  e = defaultEK
  in e { coord=xy, kind=Mushroom, prop=mkProp "Mushroom" "," }
mkEntity Potion xy    = let
  e = defaultEK
  in e { coord=xy, kind=Potion, prop=mkProp "Potion" "!" }
mkEntity StairDown xy = let
  e = defaultEK
  in e { coord=xy, kind=StairDown, prop=mkProp "Stair" ">" }
mkEntity StairUp xy   = let
  e = defaultEK
  in e { coord=xy, kind=Trap, prop=mkProp "Stair" "<" }
mkEntity Trap xy      = let
  e = defaultEK
  in e { coord=xy, kind=Trap, prop=mkProp "Trap" "^" }
mkEntity Unknown xy   = let
  e = defaultEK
  in e { coord=xy }
