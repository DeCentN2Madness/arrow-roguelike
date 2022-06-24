{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Spawn.hs

Game.Kind.Spawn.hs creates Items and Monsters...

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Spawn (defaultEK, mkItem, mkMonster) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Kind.Entity
import Game.Kind.Beasts
import Game.Kind.Visual

type Coord = (Int, Int)

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
  | n == "Player"       = [("Arrow",1),("Potion",1),("Mushroom",1),("Coin",1)]
  | n == "Necromancer"  = [("Arrow",1),("Potion",1),("Mushroom",1),("Item",1)]
  | n == "Red Dragon"   = [("Arrow",1),("Coin",1)]
  | n == "Green Dragon" = [("Arrow",1),("Coin",1)]
  | n == "Blue Dragon"  = [("Arrow",1),("Coin",1)]
  | n == "Black Dragon" = [("Arrow",1),("Coin",1)]
  | n == "White Dragon" = [("Arrow",1),("Coin",1)]
  | n == "Spider"       = [("Arrow",1),("Coin",1)]
  | n == "Wyvern"       = [("Arrow",1),("Coin",1)]
  | n == "Goblin"            = [("Arrow",1),("Mushroom",1),("Item",1)]
  | n == "Goblin Ratcatcher" = [("Arrow",1),("Mushroom",1),("Item",1)]
  | n == "Goblin Wizard"     = [("Arrow",1),("Potion",1),("Item",1)]
  | n == "Orc"               = [("Arrow",1),("Mushroom",1),("Item",1)]
  | n == "Orc Beastmaster"   = [("Arrow",1),("Mushroom",1),("Item",1)]
  | n == "Orc Shaman"        = [("Arrow",1),("Potion",1),("Item",1)]
  | n == "Troll" = [("Arrow",1),("Mushroom",1),("Item",1)]
  | otherwise = [("Arrow",0),("Potion",0),("Mushroom",0),("Coin",1)]

-- | mkItem
mkItem :: Text -> Text -> Coord -> EntityKind
mkItem n desc xy
  | n == "Arrow"     = mkEntity Arrow n desc xy
  | n == "Coin"      = mkEntity Coin n desc xy
  | n == "Corpse"    = mkEntity Corpse n desc xy
  | n == "Mushroom"  = mkEntity Mushroom n desc xy
  | n == "Potion"    = mkEntity Potion n desc xy
  | n == "StairDown" = mkEntity StairDown n desc xy
  | n == "StairUp"   = mkEntity StairUp n desc xy
  | n == "Trap"      = mkEntity Trap n desc xy
  | otherwise        = mkEntity Item n desc xy

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
    | n == "3 Hydra"      = lgMonster
    | n == "Goblin"            = smHumanoid
    | n == "Goblin Ratcatcher" = smHumanoidS
    | n == "Goblin Wizard"     = smHumanoidM
    | n == "Mouse"             = smBeast
    | n == "Necromancer"       = mdWizard
    | n == "Orc"               = mdHumanoid
    | n == "Orc Beastmaster"   = mdHumanoidS
    | n == "Orc Shaman"        = mdHumanoidM
    | n == "Ogre"              = lgHumanoid
    | n == "Skeleton"          = mdUndead
    | n == "Spider"            = lgSpider
    | n == "Troll"             = gtHumanoid
    | n == "Wolf"              = mdBeast
    | n == "Dire Wolf"         = lgBeast
    | n == "Wyvern"            = lgDragon
    | n == "Zombie"            = mdUndeadZ
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
        , eMaxMP    = mMP
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
    | count "Actor"       n > 0 = VActor
    | count "Player"      n > 0 = VActor
    | count "Dire"        n > 0 = VDire
    | count "Dragon"      n > 0 = VDragon
    | count "Hydra"       n > 0 = VHydra
    | count "Goblin"      n > 0 = VGoblin
    | count "Mouse"       n > 0 = VMouse
    | count "Orc"         n > 0 = VOrc
    | count "Ogre"        n > 0 = VOgre
    | count "Necromancer" n > 0 = VHuman
    | count "Skeleton"    n > 0 = VSkeleton
    | count "Spider"      n > 0 = VSpider
    | count "Troll"       n > 0 = VTroll
    | count "Wolf"        n > 0 = VWolf
    | count "Wyvern"      n > 0 = VLDragon
    | count "Zombie"      n > 0 = VZombie
    | count "melee"       n > 0 = VDagger
    | count "shoot"       n > 0 = VBow
    | count "jewelry"     n > 0 = VRing
    | count "neck"        n > 0 = VAmulet
    | count "armor"       n > 0 = VArmor
    | count "cloak"       n > 0 = VCloak
    | count "shield"      n > 0 = VShield
    | count "head"        n > 0 = VHelmet
    | count "hands"       n > 0 = VGloves
    | count "feet"        n > 0 = VBoots
    | otherwise = VMouse
  in visual name
