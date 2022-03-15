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
  | Arrow
  | Coin
  | Corpse
  | Item
  | Monster
  | Mushroom
  | Potion
  | StairDown
  | StairUp
  | Trap
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
  , kind      = Arrow
  , moveT     = []
  , property  = mkProp name desc xy
  , inventory = Map.empty
  , eLvl      = 0
  , eHP       = 0
  , eMaxHP    = 0
  , eXP       = 0
  }

-- | cleric
cleric :: Properties -> Properties
cleric p = let
  mProp = Map.toList p
  stats = [ ("str", "10")
          , ("dex", "12")
          , ("con", "13")
          , ("int", "14")
          , ("wis", "15")
          , ("HP", "8")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ] ++ mProp
  in Map.fromList stats

-- | fighter
fighter :: Properties -> Properties
fighter p = let
  mProp = Map.toList p
  stats = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "12")
          , ("wis", "10")
          , ("HP", "10")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ] ++ mProp
  in Map.fromList stats


-- | ranger
ranger :: Properties -> Properties
ranger p = let
  mProp = Map.toList p
  stats = [ ("str", "12")
          , ("dex", "15")
          , ("con", "13")
          , ("int", "10")
          , ("wis", "14")
          , ("HP", "10")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ] ++ mProp
  in Map.fromList stats

-- | rogue
rogue :: Properties -> Properties
rogue p = let
  mProp = Map.toList p
  stats = [ ("str", "10")
          , ("dex", "15")
          , ("con", "13")
          , ("int", "14")
          , ("wis", "12")
          , ("HP", "8")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ] ++ mProp
  in Map.fromList stats

-- | wizard
wizard :: Properties -> Properties
wizard p = let
  mProp = Map.toList p
  stats = [ ("str", "10")
          , ("dex", "12")
          , ("con", "13")
          , ("int", "15")
          , ("wis", "14")
          , ("HP", "6")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
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
          , ("Proficiency", "2")
          , ("Challenge", "1")
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
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ] ++ mProp
  in Map.fromList stats

-- | Dragon Wyrmling
mdDragon :: Properties -> Properties
mdDragon p = let
  mProp = Map.toList p
  stats = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "10")
          , ("wis", "11")
          , ("HP", "33")
          , ("XP", "450")
          , ("Proficiency", "4")
          , ("Challenge", "4")
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
          , ("Proficiency", "3")
          , ("Challenge", "3")
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
          , ("Proficiency", "2")
          , ("Challenge", "2")
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
          , ("Proficiency", "3")
          , ("Challenge", "5")
          ] ++ mProp
  in Map.fromList stats

-- | mkMonster
mkMonster :: String -> String -> Coord -> EntityKind
mkMonster name desc xy = let
  e = defaultEK xy name desc
  monster = case name of
    "Cleric"  -> cleric (property e)
    "Fighter" -> fighter (property e)
    "Mouse"   -> smBeast (property e)
    "Wolf"    -> mdBeast (property e)
    "Spider"  -> lgBeast (property e)
    "Dragon"  -> mdDragon (property e)
    "Player"  -> fighter (property e)
    "Orc"     -> mdHumanoid (property e)
    "Ranger"  -> ranger (property e)
    "Rogue"   -> rogue (property e)
    "Troll"   -> gtHumanoid (property e)
    "Wizard"  -> wizard (property e)
    _         -> fighter (property e)
  mHP  = read $ Map.findWithDefault "1" "HP" monster :: Int
  mXP  = read $ Map.findWithDefault "1" "XP" monster :: Int
  mLvl = read $ Map.findWithDefault "1" "Challenge" monster :: Int
  in e { block=True
       , kind=Monster
       , property=monster
       , eHP=mHP
       , eMaxHP=mHP
       , eXP=mXP
       , eLvl=mLvl
       }

-- | mkProp
mkProp :: String -> String -> Coord -> Properties
mkProp name desc xy = Map.fromList [ ("Name", name)
                                   , ("Description", desc)
                                   , ("spawn", show xy) ]

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy = let
  e = mkMonster "Player" "The Hero '@'" xy
  in e { kind=Actor }
mkEntity Arrow xy = let
  e = defaultEK xy "Arrow" "~"
  in e { kind=Arrow }
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
