{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Asset.hs

Game.Kind.Asset keeps the assets in the Game.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Asset (mkAssetMap) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Game.Kind.Entity

type AssetMap = EntityMap
type EntityMap = Map Int EntityKind

-- | mkAssetMap
-- All the assets within the World by Ix
-- Example:
--   1. Armor and Shields
--      a. glyph : AC : Weight
mkAssetMap :: AssetMap
mkAssetMap = let
  pos = (0, 0)
  em = [ mkMonster "Player" "The Hero (@)" pos
       -- generics
       , mkItem "Arrow"     "~" pos
       , mkItem "Coin"      "$" pos
       , mkItem "Corpse"    "%" pos
       , mkItem "Mushroom"  "," pos
       , mkItem "Potion"    "!" pos
       , mkItem "StairDown" ">" pos
       , mkItem "StairUp"   "<" pos
       , mkItem "Trap"      "^" pos
       --- melee
       , mkItem "melee/Dagger"     "|" pos
       , mkItem "melee/Sword"      "|" pos
       , mkItem "melee/Axe"        "|" pos
       , mkItem "melee/Spear"      "|" pos
       , mkItem "melee/Warhammer"  "|" pos
       , mkItem "melee/Zweihander" "|" pos
       -- shield
       , mkItem "shield/Buckler"      "):2:6" pos
       , mkItem "shield/Shield"       "):2:6" pos
       , mkItem "shield/Kite Shield"  "):2:6" pos
       -- shoot
       , mkItem "shoot/Bow"      "}" pos
       , mkItem "shoot/Longbow"  "}" pos
       , mkItem "shoot/Shortbow" "}" pos
       -- armor
       , mkItem "armor/Leather Jerkin"  "[:11:8"  pos
       , mkItem "armor/Leather Jack"    "[:11:10" pos
       , mkItem "armor/Boiled Leather"  "[:12:12" pos
       , mkItem "armor/Chain Shirt"     "[:13:20" pos
       , mkItem "armor/Scale Mail"      "[:14:45" pos
       , mkItem "armor/Breastplate"     "[:14:20" pos
       , mkItem "armor/Half Plate"      "[:15:40" pos
       , mkItem "armor/Ring Mail"       "[:14:40" pos
       , mkItem "armor/Chain Mail"      "[:16:55" pos
       , mkItem "armor/Splint"          "[:17:60" pos
       , mkItem "armor/Plate"           "[:18:65" pos
       -- head
       , mkItem "head/Leather Skullcap" "]" pos
       , mkItem "head/Mail Coif"        "]" pos
       , mkItem "head/Open Helm"        "]" pos
       , mkItem "head/Helm"             "]" pos
       -- feet
       , mkItem "feet/Boots"            "]" pos
       , mkItem "feet/Leather Leggings" "]" pos
       , mkItem "feet/Mail Chausses"    "]" pos
       , mkItem "feet/Plate Leggings"   "]" pos
       -- hands
       , mkItem "hands/Gloves"          "]" pos
       , mkItem "hands/Bracers"         "]" pos
       , mkItem "hands/Gauntlets"       "]" pos
       -- Items
       , mkItem "jewelry/Ring" "=" pos
       , mkItem "neck/Amulet"  "\"" pos
       , mkItem "cloak/Cloak" "(" pos
       , mkItem "cloak/Cape"  "(" pos
       -- monsters
       , mkMonster "Cleric"  "Medium human (h)" pos
       , mkMonster "Fighter" "Medium human (h)" pos
       , mkMonster "Ranger"  "Medium human (h)" pos
       , mkMonster "Rogue"   "Medium human (h)" pos
       , mkMonster "Mage"    "Medium human (h)" pos
       , mkMonster "Mouse"        "Small beast (r)" pos
       , mkMonster "Orc"          "Medium humanoid (o)" pos
       , mkMonster "Orc Archer"   "Medium humanoid (o)" pos
       , mkMonster "Orc Shaman"   "Medium humanoid (o)" pos
       , mkMonster "Spider"       "Large beast (S)" pos
       , mkMonster "Troll"        "Large giant (T)" pos
       , mkMonster "Wolf"         "Medium beast (c)" pos
       , mkMonster "Red Dragon"   "Medium dragon (d)" pos
       , mkMonster "Green Dragon" "Medium dragon (d)" pos
       , mkMonster "Blue Dragon"  "Medium dragon (d)" pos
       , mkMonster "Black Dragon" "Medium dragon (d)" pos
       , mkMonster "White Dragon" "Medium dragon (d)" pos
       ]
  in Map.fromList $ zip [0..] em
