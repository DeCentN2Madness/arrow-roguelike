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
fighterProp :: Properties -> Properties
fighterProp p = let
  mProp = Map.toList p
  stats = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "12")
          , ("wis", "10")
          , ("HP", "10")
          , ("XP", "0")
          , ("Weapon", "1d6")
          , ("Proficiency", "2")
          ] ++ mProp
  in Map.fromList stats

-- | Mouse
smBeast :: Properties -> Properties
smBeast p = let
  mProp = Map.toList p
  stats = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          , ("HP", "7")
          , ("XP", "25")
          , ("Weapon", "1d4")
          , ("Proficiency", "2")
          ] ++ mProp
  in Map.fromList stats

-- | Wolf
mdBeast :: Properties -> Properties
mdBeast p = let
  mProp = Map.toList p
  stats = [ ("str", "12")
          , ("dex", "15")
          , ("con", "12")
          , ("int", "3")
          , ("wis", "12")
          , ("HP", "11")
          , ("XP", "50")
          , ("Weapon", "2d4")
          , ("Proficiency", "2")
          ] ++ mProp
  in Map.fromList stats

-- | Spider
lgBeast :: Properties -> Properties
lgBeast p = let
  mProp = Map.toList p
  stats = [ ("str", "14")
          , ("dex", "16")
          , ("con", "12")
          , ("int", "2")
          , ("wis", "11")
          , ("HP", "26")
          , ("XP", "200")
          , ("Weapon", "1d8")
          , ("Proficiency", "3")
          ] ++ mProp
  in Map.fromList stats

-- | Orc
mdHumanoid :: Properties -> Properties
mdHumanoid p = let
  mProp = Map.toList p
  stats = [ ("str", "16")
          , ("dex", "12")
          , ("con", "16")
          , ("int", "7")
          , ("wis", "11")
          , ("HP", "15")
          , ("XP", "100")
          , ("Weapon", "1d12")
          , ("Proficiency", "2")
          ] ++ mProp
  in Map.fromList stats

-- | Troll
gtHumanoid :: Properties -> Properties
gtHumanoid p = let
  mProp = Map.toList p
  stats = [ ("str", "18")
          , ("dex", "13")
          , ("con", "20")
          , ("int", "7")
          , ("wis", "9")
          , ("HP", "84")
          , ("XP", "1800")
          , ("Weapon", "2d6")
          , ("Proficiency", "3")
          ] ++ mProp
  in Map.fromList stats

-- | mkMonster
mkMonster :: String -> String -> Coord -> EntityKind
mkMonster name desc xy = let
  e = defaultEK xy name desc
  p = case name of
    "Mouse"  -> smBeast (property e)
    "Spider" -> lgBeast (property e)
    "Wolf"   -> mdBeast (property e)
    "Orc"    -> mdHumanoid (property e)
    "Troll"  -> gtHumanoid (property e)
    "Player" -> fighterProp (property e)
    _        -> fighterProp (property e)
  mHP = read $ Map.findWithDefault "1" "HP" p :: Int
  mXP = read $ Map.findWithDefault "1" "XP" p :: Int
  in e { block=True
       , kind=Monster
       , property=p
       , eHP=mHP
       , eMaxHP=mHP
       , eXP=mXP
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
  e = mkMonster "Player" "@" xy
  in e { kind=Actor }
mkEntity Coin xy = let
  e = defaultEK xy "Coin" "$"
  in e { kind=Coin }
mkEntity Corpse xy = let
  e = defaultEK xy "Corpse" "%"
  in e { kind=Corpse }
mkEntity Item xy = let
  e = defaultEK xy "Item" "["
  in e { kind=Item }
mkEntity Monster xy = mkMonster "Orc" "Medium humanoid (o)" xy
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
  e = defaultEK xy "StairUp" "<"
  in e { kind=StairUp }
mkEntity Trap xy = let
  e = defaultEK xy "Trap" "^"
  in e { kind=Trap }
mkEntity Unknown xy = let
  e = defaultEK xy "Unknown" "~"
  in e { kind=Unknown }
