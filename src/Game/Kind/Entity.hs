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
          , ("dex", "13")
          , ("con", "12")
          , ("int", "14")
          , ("wis", "10")
          ] ++ mProp
  in Map.fromList stats

-- | monsterProp
orcProp :: Properties -> Properties
orcProp p = let
  mProp = Map.toList p
  stats = [ ("str", "16")
          , ("dex", "12")
          , ("con", "16")
          , ("int", "7")
          , ("wis", "11")
          , ("HP", "15")
          , ("XP", "100")
          ] ++ mProp
  in Map.fromList stats

-- | mouseProp
mouseProp :: Properties -> Properties
mouseProp p = let
  mProp = Map.toList p
  stats = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          , ("HP", "7")
          , ("XP", "25")
          ] ++ mProp
  in Map.fromList stats

-- | spiderProp
spiderProp :: Properties -> Properties
spiderProp p = let
  mProp = Map.toList p
  stats = [ ("str", "14")
          , ("dex", "16")
          , ("con", "12")
          , ("int", "2")
          , ("wis", "11")
          , ("HP", "26")
          , ("XP", "200")
          ] ++ mProp
  in Map.fromList stats

-- | mkMonster
mkMonster :: String -> String -> Coord -> EntityKind
mkMonster name desc xy = let
  e = defaultEK xy name desc
  p = case name of
    "Mouse"  -> mouseProp (property e)
    "Orc"    -> orcProp (property e)
    "Spider" -> spiderProp (property e)
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
  e = defaultEK xy "Player" "@"
  in e { block=True
       , kind=Actor
       , property=fighterProp (property e)
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
  e = defaultEK xy "Orc" "Medium humanoid (o)"
  in e { block=True
       , kind=Monster
       , property=orcProp (property e)
       , eHP=15
       , eXP=100
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
  e = defaultEK xy "StairUp" "<"
  in e { kind=Trap }
mkEntity Trap xy = let
  e = defaultEK xy "Trap" "^"
  in e { kind=Trap }
mkEntity Unknown xy = let
  e = defaultEK xy "Unknown" "~"
  in e { kind=Unknown }
