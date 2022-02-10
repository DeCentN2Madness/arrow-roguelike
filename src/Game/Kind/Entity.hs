{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..)
                        , EntityKind(..)
                        , mkEntity
                        , Properties) where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Map (Map)
import GHC.Generics
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)
type Properties = Map String String
type Inventories = Map String Int

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
  deriving (Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

data EntityKind = EntityKind
  { coord      :: Coord
  , block      :: Bool
  , kind       :: Entity
  , moveT      :: [Coord] -- where can the Entity move?
  , property   :: Properties
  , inventory  :: Inventories
  , eLvl       :: Int
  , eHP        :: Int
  , eMaxHP     :: Int
  , eXP        :: Int
  } deriving (Show, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

defaultEK :: EntityKind
defaultEK = EntityKind {
  coord = (0,0)
  , block = False
  , kind = Unknown
  , moveT = []
  , property = mkProp "zero" "0"
  , inventory = Map.empty
  , eLvl = 0
  , eHP = 0
  , eMaxHP = 0
  , eXP = 0
  }

-- | fighterProp
fighterProp :: Properties
fighterProp = let
  std = Map.toList $ mkProp "Player" "@"
  stats = [ ("str", "15")
          , ("int", "14")
          , ("dex", "13")
          , ("con", "12")
          , ("wis", "10")
          ] ++ std
  in Map.fromList stats

-- | mouseProp
mouseProp :: Properties
mouseProp = let
  std = Map.toList $ mkProp "Mouse" "r"
  stats = [ ("str", "10")
          , ("int", "15")
          , ("dex", "11")
          , ("con", "2")
          , ("wis", "10")
          ] ++ std
  in Map.fromList stats

-- | mkProp
mkProp :: String -> String -> Properties
mkProp x y = Map.fromList [("Name", x), ("Desc", y)]

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy = let
  e = defaultEK
  in e { coord=xy
       , block=True
       , kind=Actor
       , property=fighterProp
       , eHP=10
       , eMaxHP=10
       , eLvl=1 }
mkEntity Coin xy = let
  e = defaultEK
  in e { coord=xy, kind=Coin, property=mkProp "Coin" "$" }
mkEntity Corpse xy = let
  e = defaultEK
  in e { coord=xy, kind=Corpse, property=mkProp "Corpse" "%" }
mkEntity Item xy = let
  e = defaultEK
  in e { coord=xy, kind=Item, property=mkProp "Item" "[" }
mkEntity Mouse xy = let
  e = defaultEK
  in e { coord=xy
       , block=True
       , kind=Mouse
       , property=mouseProp
       , eHP=7
       , eXP=35
       , eLvl=1
       }
mkEntity Mushroom xy = let
  e = defaultEK
  in e { coord=xy, kind=Mushroom, property=mkProp "Mushroom" "," }
mkEntity Potion xy = let
  e = defaultEK
  in e { coord=xy, kind=Potion, property=mkProp "Potion" "!" }
mkEntity StairDown xy = let
  e = defaultEK
  in e { coord=xy, kind=StairDown, property=mkProp "Stair" ">" }
mkEntity StairUp xy = let
  e = defaultEK
  in e { coord=xy, kind=Trap, property=mkProp "Stair" "<" }
mkEntity Trap xy = let
  e = defaultEK
  in e { coord=xy, kind=Trap, property=mkProp "Trap" "^" }
mkEntity Unknown xy = let
  e = defaultEK
  in e { coord=xy }
