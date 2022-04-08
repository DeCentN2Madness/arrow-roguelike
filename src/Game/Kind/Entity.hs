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
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Game.Kind.Visual (VisualKind(..))

type Coord = (Int, Int)
type Prop = [(Text, Text)]
type Properties = Map Text Text
type Inventory = Map Text Int

-- | Entity stack sort...
data Entity
  = Actor
  | Monster
  | StairDown
  | StairUp
  | Coin
  | Corpse
  | Arrow
  | Mushroom
  | Item
  | Potion
  | Trap
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

-- | EntityKind
-- coord     = Entity Position
-- block     = Movable Entity
-- kind      = Kind of Entity
-- glyph     = VisualKind of Entity
-- moveT     = Where can the Entity move?
-- spawn     = Where can the Entity spawn?
-- property  = Textual descriptions of the entity
-- inventory = Items
-- eLvl      = Level of the entity
-- eHP       = HitPoint
-- eMaxHP    = Max HP
-- eMP       = ManaPoint
-- eMaxMP    = Max MP
-- eXP       = Experience
data EntityKind = EntityKind
  { coord      :: Coord
  , block      :: Bool
  , kind       :: Entity
  , glyph      :: VisualKind
  , moveT      :: [Coord]
  , spawn      :: Coord
  , property   :: Properties
  , inventory  :: Inventory
  , eLvl       :: Int
  , eHP        :: Int
  , eMaxHP     :: Int
  , eMP        :: Int
  , eMaxMP     :: Int
  , eXP        :: Int
  } deriving (Show, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

defaultEK :: Text -> Text -> Coord -> EntityKind
defaultEK name desc xy =
  EntityKind { coord = xy
             , block     = False
             , kind      = Arrow
             , glyph     = VArrow
             , moveT     = []
             , spawn     = xy
             , property  = mkProp name desc []
             , inventory = Map.empty
             , eLvl      = 0
             , eHP       = 0
             , eMaxHP    = 0
             , eMP       = 0
             , eMaxMP    = 0
             , eXP       = 0
             }

-- | mkInventory
-- One lucky Mushroom
mkInventory :: Text -> [(Text, Int)]
mkInventory n
  | n == "Cleric"  = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Fighter" = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Mage"    = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Player"  = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",0)]
  | n == "Ranger"  = [("Arrow",1),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Rogue"   = [("Arrow",0),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Dragon"     = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Orc"        = [("Arrow",0),("Potion",0),("Mushroom",5),("Coin",1)]
  | n == "Orc Archer" = [("Arrow",5),("Potion",0),("Mushroom",0),("Coin",1)]
  | n == "Orc Shaman" = [("Arrow",0),("Potion",5),("Mushroom",0),("Coin",1)]
  | n == "Spider"     = [("Arrow",0),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Troll"        = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Troll Archer" = [("Arrow",1),("Potion",0),("Mushroom",0),("Coin",1)]
  | n == "Troll Shaman" = [("Arrow",0),("Potion",1),("Mushroom",0),("Coin",1)]
  | otherwise     = [("Arrow",0),("Potion",0),("Mushroom",0),("Coin",0)]

-- | mkMonster
mkMonster :: Text -> Text -> Coord -> EntityKind
mkMonster name desc xy = let
  e = defaultEK name desc xy
  monster = case name of
    "Cleric"  -> cleric
    "Dragon"  -> mdDragon
    "Fighter" -> fighter
    "Mage"    -> mage
    "Mouse"   -> smBeast
    "Orc"     -> mdHumanoid
    "Orc Archer" -> mdHumanoid
    "Orc Shaman" -> mdHumanoid
    "Player"  -> fighter
    "Ranger"  -> ranger
    "Rogue"   -> rogue
    "Spider"  -> lgBeast
    "Troll"   -> gtHumanoid
    "Troll Archer" -> gtHumanoid
    "Troll Shaman" -> gtHumanoid
    "Wolf"    -> mdBeast
    _         -> fighter
  mProp = mkProp name desc monster
  mHP  = read $ T.unpack $ Map.findWithDefault "1" "HP" mProp
  mXP  = read $ T.unpack $ Map.findWithDefault "1" "XP" mProp
  mLvl = read $ T.unpack $ Map.findWithDefault "1" "Challenge" mProp
  mInv = Map.fromList $ mkInventory name
  in e { block=True
       , kind=Monster
       , glyph=visualId name
       , property=mProp
       , inventory=mInv
       , eLvl=mLvl
       , eHP=mHP
       , eMaxHP=mHP
       , eXP=mXP
       }

-- | mkProp
mkProp :: Text -> Text -> Prop -> Properties
mkProp name desc p = Map.fromList $
  [ ("Name", name), ("Description", desc)] ++ p

-- | mkEntity
mkEntity :: Entity -> Coord -> EntityKind
mkEntity Actor xy = let
  e = mkMonster "Player" "The Hero '@'" xy
  in e { kind=Actor, glyph=VActor }
mkEntity Arrow xy = let
  e = defaultEK "Arrow" "~" xy
  in e { kind=Arrow, glyph=VArrow }
mkEntity Coin xy = let
  e = defaultEK "Coin" "$" xy
  in e { kind=Coin, glyph=VCoin }
mkEntity Corpse xy = let
  e = defaultEK "Corpse" "%" xy
  in e { kind=Corpse, glyph=VCorpse }
mkEntity Item xy = let
  e = defaultEK "Item" "[" xy
  in e { kind=Item, glyph=VItem }
mkEntity Monster xy = mkMonster "Orc" "Medium humanoid (o)" xy
mkEntity Mushroom xy = let
  e = defaultEK "Mushroom" "," xy
  in e { kind=Mushroom, glyph=VMushroom }
mkEntity Potion xy = let
  e = defaultEK "Potion" "!" xy
  in e { kind=Potion, glyph=VPotion }
mkEntity StairDown xy = let
  e = defaultEK "StairDown" ">" xy
  in e { kind=StairDown, glyph=VStairDn }
mkEntity StairUp xy = let
  e = defaultEK "StairUp" "<" xy
  in e { kind=StairUp, glyph=VStairUp }
mkEntity Trap xy = let
  e = defaultEK "Trap" "^" xy
  in e { kind=Trap, glyph=VTrap }

-- | identify Item, Monster, ... by Name
visualId :: Text -> VisualKind
visualId name = let
  count x xs = length $ filter (==T.pack x) (T.words xs)
  visual n
    | count "Dragon" n > 0 = VDragon
    | count "Orc"    n > 0 = VOrc
    | count "Spider" n > 0 = VSpider
    | count "Troll"  n > 0 = VTroll
    | count "Wolf"   n > 0 = VWolf
    | otherwise            = VMouse
  in visual name

-- | cleric
cleric :: Prop
cleric = [ ("str", "10")
         , ("dex", "12")
         , ("con", "13")
         , ("int", "14")
         , ("wis", "15")
         , ("HP", "8")
         , ("XP", "0")
         , ("Proficiency", "2")
         , ("Challenge", "1")
         , ("Throw", "chants!")
         ]

-- | fighter
fighter :: Prop
fighter = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "12")
          , ("wis", "10")
          , ("HP", "10")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ]

-- | mage
mage :: Prop
mage = [ ("str", "10")
       , ("dex", "12")
       , ("con", "13")
       , ("int", "15")
       , ("wis", "14")
       , ("HP", "6")
       , ("XP", "0")
       , ("Proficiency", "2")
       , ("Challenge", "1")
       , ("Throw", "casts!")
       ]

-- | ranger
ranger :: Prop
ranger = [ ("str", "12")
         , ("dex", "15")
         , ("con", "13")
         , ("int", "10")
         , ("wis", "14")
         , ("HP", "10")
         , ("XP", "0")
         , ("Proficiency", "2")
         , ("Challenge", "1")
         ]

-- | rogue
rogue :: Prop
rogue = [ ("str", "10")
        , ("dex", "15")
        , ("con", "13")
        , ("int", "14")
        , ("wis", "12")
        , ("HP", "8")
        , ("XP", "0")
        , ("Proficiency", "2")
        , ("Challenge", "1")
        ]

-- | Mouse
smBeast :: Prop
smBeast = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          , ("HP", "7")
          , ("XP", "25")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ]

-- | Wolf
mdBeast :: Prop
mdBeast = [ ("str", "12")
          , ("dex", "15")
          , ("con", "12")
          , ("int", "3")
          , ("wis", "12")
          , ("HP", "11")
          , ("XP", "50")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          ]

-- | Dragon Wyrmling
mdDragon :: Prop
mdDragon = [ ("str", "15")
           , ("dex", "14")
           , ("con", "13")
           , ("int", "10")
           , ("wis", "11")
           , ("HP", "33")
           , ("XP", "450")
           , ("Proficiency", "4")
           , ("Challenge", "4")
           , ("Throw", "breathes!")
           ]

-- | Spider
lgBeast :: Prop
lgBeast = [ ("str", "14")
          , ("dex", "16")
          , ("con", "12")
          , ("int", "2")
          , ("wis", "11")
          , ("HP", "26")
          , ("XP", "200")
          , ("Proficiency", "3")
          , ("Challenge", "3")
          ]

-- | Orc
mdHumanoid :: Prop
mdHumanoid = [ ("str", "16")
             , ("dex", "12")
             , ("con", "16")
             , ("int", "7")
             , ("wis", "11")
             , ("HP", "15")
             , ("XP", "100")
             , ("Proficiency", "2")
             , ("Challenge", "2")
             ]

-- | Troll
gtHumanoid :: Prop
gtHumanoid = [ ("str", "18")
             , ("dex", "13")
             , ("con", "20")
             , ("int", "7")
             , ("wis", "9")
             , ("HP", "84")
             , ("XP", "1800")
             , ("Proficiency", "3")
             , ("Challenge", "5")
             ]
