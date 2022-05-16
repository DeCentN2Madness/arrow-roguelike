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
defaultEK name desc pos =
  EntityKind { coord     = pos
             , block     = False
             , kind      = Arrow
             , glyph     = VArrow
             , moveT     = []
             , spawn     = pos
             , property  = mkProp name desc []
             , inventory = Map.empty
             , eLvl      = 1
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
  | n == "Player"       = [("Arrow",0),("Potion",0),("Mushroom",1),("Coin",0)]
  | n == "Red Dragon"   = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Green Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Blue Dragon"  = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Black Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "White Dragon" = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Orc"          = [("Arrow",0),("Potion",0),("Mushroom",1),("Item",1)]
  | n == "Orc Archer"   = [("Arrow",3),("Potion",0),("Mushroom",1),("Item",1)]
  | n == "Orc Shaman"   = [("Arrow",1),("Potion",0),("Mushroom",1),("Item",1)]
  | n == "Spider"       = [("Arrow",1),("Potion",1),("Mushroom",1),("Item",1)]
  | n == "Troll"        = [("Arrow",1),("Potion",1),("Mushroom",1),("Item",1)]
  | otherwise           = [("Arrow",0),("Potion",0),("Mushroom",0),("Coin",0)]

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
    | n == "Player"       = fighter
    | n == "Red Dragon"   = mdDragon
    | n == "Green Dragon" = mdDragon
    | n == "Blue Dragon"  = mdDragon
    | n == "Black Dragon" = mdDragon
    | n == "White Dragon" = mdDragon
    | n == "Mouse"        = smBeast
    | n == "Orc"          = mdHumanoid
    | n == "Orc Archer"   = mdHumanoidA
    | n == "Orc Shaman"   = mdHumanoidM
    | n == "Ogre"         = lgHumanoid
    | n == "Spider"       = lgSpider
    | n == "Troll"        = gtHumanoid
    | n == "Wolf"         = mdBeast
    | n == "Dire Wolf"    = lgBeast
    | otherwise = mdHumanoid
  mProp = mkProp name desc (monster name)
  mHP  = read $ T.unpack $ Map.findWithDefault "1" "HP" mProp
  mMP  = read $ T.unpack $ Map.findWithDefault "0" "MP" mProp
  mXP  = read $ T.unpack $ Map.findWithDefault "1" "XP" mProp
  mInv = Map.fromList $ mkInventory name
  ek = defaultEK name desc xy
  in ek { block     = True
        , kind      = if name == "Player" then Actor else Monster
        , glyph     = visualId name
        , property  = mProp
        , inventory = mInv
        , eHP       = if mHP > 1 then mHP else 1
        , eMaxHP    = if mHP > 1 then mHP else 1
        , eMP       = mMP
        , eXP       = mXP
        }

-- | mkProp
mkProp :: Text -> Text -> Prop -> Properties
mkProp name desc prop = Map.fromList $
  [ ("Name", name), ("Description", desc) ] ++ prop

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
    | count "Actor"   n > 0 = VActor
    | count "Player"  n > 0 = VActor
    | count "Dire"    n > 0 = VDire
    | count "Dragon"  n > 0 = VDragon
    | count "Mouse"   n > 0 = VMouse
    | count "Orc"     n > 0 = VOrc
    | count "Ogre"    n > 0 = VOgre
    | count "Spider"  n > 0 = VSpider
    | count "Troll"   n > 0 = VTroll
    | count "Wolf"    n > 0 = VWolf
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

-- | fighter
fighter :: Prop
fighter = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "12")
          , ("wis", "10")
          , ("HP", "10")
          , ("MP", "0")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Class", "Fighter")
          , ("AC", "11")
          , ("WT", "11")
          , ("WWT", "1")
          , ("ATTACK", "1d4")
          , ("SHOOT", "1d4")
          , ("ATTACKS", "0")
          , ("CAST", "0")
          , ("melee", "melee/Dagger")
          , ("shoot", "shoot/Sling")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Leather")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Mouse
smBeast :: Prop
smBeast = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          , ("HP", "7")
          , ("MP", "0")
          , ("XP", "25")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "12")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "1d4")
          , ("SHOOT", "1d1")
          , ("ATTACKS", "0")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Squeak")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "None")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
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
          , ("Class", "Beast")
          , ("AC", "13")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "2d4")
          , ("SHOOT", "1d2")
          , ("ATTACKS", "0")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Howl")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Dire Wolf
lgBeast :: Prop
lgBeast = [ ("str", "17")
          , ("dex", "15")
          , ("con", "15")
          , ("int", "3")
          , ("wis", "12")
          , ("HP", "37")
          , ("MP", "0")
          , ("XP", "200")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "14")
          , ("WT", "0")
          , ("WWT", "10")
          , ("ATTACK", "2d5")
          , ("SHOOT",  "1d2")
          , ("ATTACKS", "0")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Howl")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Dragon Wyrmling
mdDragon :: Prop
mdDragon = [ ("str", "15")
           , ("dex", "12")
           , ("con", "13")
           , ("int", "14")
           , ("wis", "11")
           , ("HP", "38")
           , ("MP", "0")
           , ("XP", "450")
           , ("Proficiency", "2")
           , ("Class", "Dragon")
           , ("AC", "17")
           , ("WT", "0")
           , ("WWT", "10")
           , ("ATTACK", "1d10")
           , ("SHOOT", "1d8")
           , ("ATTACKS", "1d6")
           , ("CAST", "0")
           , ("Throw", "breathes!")
           , ("melee", "melee/Bite")
           , ("shoot", "shoot/Breath")
           , ("jewelry", "None")
           , ("neck", "None")
           , ("armor", "armor/Natural Armor")
           , ("cloak", "None")
           , ("shield", "None")
           , ("head", "None")
           , ("hands", "None")
           , ("feet", "None")
           ]

-- | Spider
lgSpider :: Prop
lgSpider = [ ("str", "14")
          , ("dex", "16")
          , ("con", "12")
          , ("int", "2")
          , ("wis", "11")
          , ("HP", "26")
          , ("MP", "0")
          , ("XP", "200")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "14")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "1d8")
          , ("SHOOT",  "1d6")
          , ("ATTACKS", "1d3")
          , ("CAST", "0")
          , ("Throw", "spits!")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Web")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
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
             , ("Class", "Humanoid")
             , ("AC", "13")
             , ("WT", "21")
             , ("WWT", "7")
             , ("ATTACK", "1d12")
             , ("SHOOT", "1d6")
             , ("ATTACKS", "0")
             , ("CAST", "0")
             , ("Throw", "throws!")
             , ("melee", "melee/Greataxe")
             , ("shoot", "shoot/Javelin")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Hide")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]

-- | Orc Archer
mdHumanoidA :: Prop
mdHumanoidA = mdHumanoid
  ++ [ ("ATTACK", "1d4")
     , ("AC", "12")
     , ("WT", "14")
     , ("WWT", "1")
     , ("melee", "melee/Dagger")
     , ("armor", "armor/Leather")
     ]

-- | Orc Shaman
mdHumanoidM :: Prop
mdHumanoidM = mdHumanoid
  ++ [ ("ATTACK", "1d6")
     , ("SHOOT", "1d4")
     , ("CAST", "2d4")
     , ("AC", "11")
     , ("WT", "5")
     , ("WWT", "4")
     , ("Throw", "curses!")
     , ("melee", "melee/Quarterstaff")
     , ("shoot", "shoot/Dart")
     , ("armor", "None")
     , ("WT", "18")
     , ("WWT", "4")
     , ("MP", "10") ]

-- | Ogre
lgHumanoid :: Prop
lgHumanoid = [ ("str", "19")
             , ("dex", "8")
             , ("con", "16")
             , ("int", "5")
             , ("wis", "7")
             , ("HP", "59")
             , ("MP", "0")
             , ("XP", "450")
             , ("Proficiency", "2")
             , ("Class", "Giant")
             , ("AC", "13")
             , ("WT", "24")
             , ("WWT", "10")
             , ("ATTACK", "2d8")
             , ("SHOOT", "1d6")
             , ("ATTACKS", "0")
             , ("CAST", "0")
             , ("Throw", "hurls!")
             , ("melee", "melee/Greatclub")
             , ("shoot", "shoot/Javelin")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Hide")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
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
             , ("Class", "Giant")
             , ("AC", "15")
             , ("WT", "21")
             , ("WWT", "10")
             , ("ATTACK", "2d6")
             , ("SHOOT", "1d8")
             , ("ATTACKS", "1d6")
             , ("CAST", "0")
             , ("Throw", "burps!")
             , ("melee", "melee/Claw")
             , ("shoot", "shoot/Spit")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Natural Armor")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]
