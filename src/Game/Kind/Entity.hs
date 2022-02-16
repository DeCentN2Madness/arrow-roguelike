{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..)
                        , EntityKind(..)
                        , mkEntity
                        , mkMonster
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
  | Monster
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

defaultEK :: (Int, Int) -> String -> String -> EntityKind
defaultEK xy name desc = EntityKind {
  coord = xy
  , block     = False
  , kind      = Unknown
  , moveT     = []
  , property  = mkProp name desc xy
  , inventory = Map.empty
  , eLvl      = 0
  , eHP       = 0
  , eMaxHP    = 0
  , eXP       = 0
  }

-- | fighterProp
fighterProp :: String -> String -> Coord -> Properties
fighterProp name desc xy = let
  std = Map.toList $ mkProp name desc xy
  stats = [ ("str", "15")
          , ("dex", "13")
          , ("con", "12")
          , ("int", "14")
          , ("wis", "10")
          ] ++ std
  in Map.fromList stats

-- | monsterProp
monsterProp :: String -> String -> Coord -> Properties
monsterProp name desc xy = let
  std = Map.toList $ mkProp name desc xy
  stats = [ ("str", "10")
          , ("dex", "10")
          , ("con", "10")
          , ("int", "10")
          , ("wis", "10")
          ] ++ std
  in Map.fromList stats

-- | mouseProp
mouseProp :: String -> String -> Coord -> Properties
mouseProp name desc xy = let
  std = Map.toList $ mkProp name desc xy
  stats = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          ] ++ std
  in Map.fromList stats

-- | mkMonster
mkMonster :: String -> Coord -> EntityKind
mkMonster name xy = let
  p = case name of
    "Mouse" -> mouseProp   name "r"  xy
    "Orc"   -> monsterProp name "o" xy
    "Human" -> fighterProp name "h" xy
    "Spider" -> monsterProp name "S" xy
    _       -> monsterProp name "o" xy
  e = defaultEK xy name "asset"
  in e { block=True
       , kind=Monster
       , property=p
       , eHP=10
       , eXP=50
       , eLvl=1
       }

-- | mkProp
mkProp :: String -> String -> Coord -> Properties
mkProp name desc xy = Map.fromList [ ("Name", name)
                                   , ("Description", desc)
                                   , ("spawn", show xy) ]

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy = let
  e = defaultEK xy "Player" "@"
  in e { block=True
       , kind=Actor
       , property=fighterProp "Player" "@" xy
       , eHP=10
       , eMaxHP=10
       , eLvl=1 }
mkEntity Coin xy = let
  e = defaultEK xy "Coin" "$"
  in e { kind=Coin }
mkEntity Corpse xy = let
  e = defaultEK xy "Corpse" "%"
  in e { kind=Corpse }
mkEntity Item xy = let
  e = defaultEK xy "Item" "["
  in e { kind=Item }
mkEntity Monster xy = let
  e = defaultEK xy "Monster" "o"
  in e { block=True
       , kind=Monster
       , property=monsterProp "Orc" "o" xy
       , eHP=10
       , eXP=50
       , eLvl=1
       }
mkEntity Mushroom xy = let
  e = defaultEK xy "Mushroom" ","
  in e { kind=Mushroom }
mkEntity Potion xy = let
  e = defaultEK xy "Potion" "!"
  in e { kind=Potion }
mkEntity StairDown xy = let
  e = defaultEK xy "StairDown" ">"
  in e { kind=StairDown }
mkEntity StairUp xy = let
  e = defaultEK xy "Stair" "<"
  in e { kind=Trap }
mkEntity Trap xy = let
  e = defaultEK xy "Trap" "^"
  in e { kind=Trap }
mkEntity Unknown xy = let
  e = defaultEK xy "Unknown" "~"
  in e { kind=Unknown }
