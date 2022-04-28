{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Entity (Entity(..)
                        , EntityKind(..)
                        , mkItem
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
  | Coin
  | Item
  | Corpse
  | Arrow
  | Mushroom
  | Potion
  | Trap
  | StairDown
  | StairUp
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
  | n == "Cleric"  = [("Arrow",0),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Fighter" = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Mage"    = [("Arrow",0),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Player"  = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",0)]
  | n == "Ranger"  = [("Arrow",1),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Rogue"   = [("Arrow",0),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Red Dragon"   = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Green Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Blue Dragon"  = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Black Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "White Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Orc"          = [("Arrow",0),("Potion",0),("Mushroom",1),("Item",1)]
  | n == "Orc Archer"   = [("Arrow",5),("Potion",0),("Mushroom",1),("Item",1)]
  | n == "Orc Shaman"   = [("Arrow",0),("Potion",1),("Mushroom",1),("Item",1)]
  | n == "Spider"       = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",1)]
  | n == "Troll"        = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",1)]
  | otherwise = [("Arrow",0),("Potion",0),("Mushroom",0),("Coin",0)]

-- | mkItem
mkItem :: Text -> Text -> Coord -> EntityKind
mkItem name desc xy = let
  item n
    | n == "Arrow"     = mkEntity Arrow name desc xy
    | n == "Coin"      = mkEntity Coin name desc xy
    | n == "Corpse"    = mkEntity Corpse name desc xy
    | n == "Mushroom"  = mkEntity Mushroom name desc xy
    | n == "Potion"    = mkEntity Potion name desc xy
    | n == "StairDown" = mkEntity StairDown name desc xy
    | n == "StairUp"   = mkEntity StairUp name desc xy
    | n == "Trap"      = mkEntity Trap name desc xy
    | otherwise        = mkEntity Item name desc xy
  in item name

-- | mkMonster
mkMonster :: Text -> Text -> Coord -> EntityKind
mkMonster name desc xy = let
  monster n
    | n == "Cleric"  = cleric
    | n == "Fighter" = fighter
    | n == "Mage"    = mage
    | n == "Player"  = fighter
    | n == "Ranger"  = ranger
    | n == "Rogue"   = rogue
    | n == "Red Dragon"   = mdDragon
    | n == "Green Dragon" = mdDragon
    | n == "Blue Dragon"  = mdDragon
    | n == "Black Dragon" = mdDragon
    | n == "White Dragon" = mdDragon
    | n == "Mouse"      = smBeast
    | n == "Orc"        = mdHumanoid
    | n == "Orc Archer" = mdHumanoid
    | n == "Orc Shaman" = mdHumanoidM
    | n == "Spider"     = lgBeast
    | n == "Troll"      = gtHumanoid
    | n == "Wolf"       = mdBeast
    | otherwise = mdHumanoid
  mProp = mkProp name desc (monster name)
  mHP  = read $ T.unpack $ Map.findWithDefault "1" "HP" mProp
  mMP  = read $ T.unpack $ Map.findWithDefault "0" "MP" mProp
  mXP  = read $ T.unpack $ Map.findWithDefault "1" "XP" mProp
  mLvl = read $ T.unpack $ Map.findWithDefault "1" "Challenge" mProp
  mInv = Map.fromList $ mkInventory name
  ek = defaultEK name desc xy
  in ek { block=True
        , kind=if name == "Player" then Actor else Monster
        , glyph=visualId name
        , property=mProp
        , inventory=mInv
        , eLvl=mLvl
        , eHP=mHP
        , eMaxHP=mHP
        , eMP=mMP
        , eXP=mXP
        }

-- | mkProp
mkProp :: Text -> Text -> Prop -> Properties
mkProp name desc p = Map.fromList $
  [ ("Name", name), ("Description", desc)] ++ p

-- | mkEntity
mkEntity :: Entity -> Text -> Text -> Coord -> EntityKind
mkEntity Actor   name desc xy = mkMonster name desc xy
mkEntity Monster name desc xy = mkMonster name desc xy
mkEntity Arrow name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Arrow, glyph=VArrow }
mkEntity Coin name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Coin, glyph=VCoin }
mkEntity Corpse name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Corpse, glyph=VCorpse }
mkEntity Item name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Item, glyph=visualId name }
mkEntity Mushroom name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Mushroom, glyph=VMushroom }
mkEntity Potion name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Potion, glyph=VPotion }
mkEntity StairDown name desc xy = let
  e = defaultEK name desc xy
  in e { kind=StairDown, glyph=VStairDn }
mkEntity StairUp name desc xy = let
  e = defaultEK name desc xy
  in e { kind=StairUp, glyph=VStairUp }
mkEntity Trap name desc xy = let
  e = defaultEK name desc xy
  in e { kind=Trap, glyph=VTrap }

-- | identify Item, Monster, ... by Name
visualId :: Text -> VisualKind
visualId name = let
  count :: Text -> Text -> Int
  count x xs = length $
    filter (==x) (T.words $ fst $ T.breakOn "/" xs)
  visual n
    | count "Actor"  n > 0 = VActor
    | count "Player" n > 0 = VActor
    | count "Dragon" n > 0 = VDragon
    | count "Mouse"  n > 0 = VMouse
    | count "Orc"    n > 0 = VOrc
    | count "Spider" n > 0 = VSpider
    | count "Troll"  n > 0 = VTroll
    | count "Wolf"   n > 0 = VWolf
    | count "melee"   n > 0 = VDagger
    | count "shoot"   n > 0 = VBow
    | count "jewelry" n > 0 = VRing
    | count "neck"    n > 0 = VAmulet
    | count "armor"   n > 0 = VArmor
    | count "cloak"   n > 0 = VCloak
    | count "shield"  n > 0 = VShield
    | count "head"    n > 0 = VHelmet
    | count "hands"   n > 0 = VGloves
    | count "feet"    n > 0 = VBoots
    | otherwise = VMouse
  in visual name

-- | cleric
cleric :: Prop
cleric = [ ("str", "10")
         , ("dex", "12")
         , ("con", "13")
         , ("int", "14")
         , ("wis", "15")
         , ("HP", "8")
         , ("MP", "10")
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
          , ("MP", "1")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Challenge", "1")
          , ("melee", "None")
          , ("shoot", "None")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "None")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | mage
mage :: Prop
mage = [ ("str", "10")
       , ("dex", "12")
       , ("con", "13")
       , ("int", "15")
       , ("wis", "14")
       , ("HP", "6")
       , ("MP", "12")
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
         , ("MP", "1")
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
        , ("MP", "2")
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
          , ("MP", "0")
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
           , ("MP", "0")
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
          , ("MP", "0")
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
             , ("MP", "0")
             , ("XP", "100")
             , ("Proficiency", "2")
             , ("Challenge", "2")
             ]

-- | Orc Mage
mdHumanoidM :: Prop
mdHumanoidM = [ ("str", "12")
              , ("dex", "12")
              , ("con", "16")
              , ("int", "11")
              , ("wis", "11")
              , ("HP", "15")
              , ("MP", "10")
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
             , ("MP", "0")
             , ("XP", "1800")
             , ("Proficiency", "3")
             , ("Challenge", "5")
             ]
