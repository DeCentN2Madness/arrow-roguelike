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
  em = [ mkMonster "Player" "The Hero (@)"  pos
       -- generics
       , mkItem "Arrow"     "Ammunition (~)"  pos
       , mkItem "Coin"      "Gold Coins ($)"  pos
       , mkItem "Corpse"    "Bones (%)"       pos
       , mkItem "Mushroom"  "Tasty Food (,)"  pos
       , mkItem "Potion"    "Yummy Drink (!)" pos
       , mkItem "StairDown" "Exit (>)"        pos
       , mkItem "StairUp"   "Exit (<)"        pos
       , mkItem "Trap"      "Danger (^)"      pos
       --- melee basic
       , mkItem "melee/Club"            "|:1d4:2:Light"          pos
       , mkItem "melee/Dagger"          "|:1d4:1:Finesse, Light" pos
       , mkItem "melee/Greatclub"       "|:1d8:10:Heavy"         pos
       , mkItem "melee/Handaxe"         "|:1d6:2:Light"          pos
       , mkItem "melee/Mace"            "|:1d6:4:"               pos
       , mkItem "melee/Pick"            "|:1d8:6:Digger"         pos
       , mkItem "melee/Quarterstaff"    "|:1d6:4:"               pos
       , mkItem "melee/Sickle"          "|:1d4:2:Light"          pos
       , mkItem "melee/Spear"           "|:1d6:3:"               pos
       , mkItem "melee/Whip"            "|:1d4:2:Finesse"        pos
       -- melee martial
       , mkItem "melee/Battleaxe"       "|:1d8:4:"               pos
       , mkItem "melee/Flail"           "|:1d8:3:"               pos
       , mkItem "melee/Glaive"          "|:1d10:6:Heavy"         pos
       , mkItem "melee/Greataxe"        "|:1d12:7:Heavy"         pos
       , mkItem "melee/Halberd"         "|:1d10:6:Heavy"         pos
       , mkItem "melee/Lance"           "|:1d12:6:Heavy"         pos
       , mkItem "melee/Longsword"       "|:1d8:3:"               pos
       , mkItem "melee/Maul"            "|:1d12:10:Heavy"        pos
       , mkItem "melee/Morningstar"     "|:1d8:4:"               pos
       , mkItem "melee/Military Flail"  "|:2d4:3:"               pos
       , mkItem "melee/Pike"            "|:1d10:18:Heavy"        pos
       , mkItem "melee/Rapier"          "|:1d8:2:Finesse"        pos
       , mkItem "melee/Scimitar"        "|:1d6:2:Finesse, Light" pos
       , mkItem "melee/Shortsword"      "|:1d6:2:Finesse, Light" pos
       , mkItem "melee/Trident"         "|:1d6:4:"               pos
       , mkItem "melee/Warhammer"       "|:1d12:6:Heavy"         pos
       , mkItem "melee/Zweihander"      "|:2d6:6:Heavy"          pos
       -- melee Elvish
       , mkItem "melee/Elvish Dagger"   "|:2d2:1:Magic"          pos
       , mkItem "melee/Elvish Spear"    "|:2d3:3:Magic"          pos
       , mkItem "melee/Elvish Sword"    "|:2d4:3:Magic"          pos
       , mkItem "melee/Wizard Staff"    "|:2d5:4:Magic"          pos
       -- shield
       , mkItem "shield/Buckler"        "):1:2:Light" pos
       , mkItem "shield/Shield"         "):2:6:"      pos
       , mkItem "shield/Kite Shield"    "):2:5:"      pos
       -- shoot
       , mkItem "shoot/Blowgun"         "}:1d1:1:Ammo"          pos
       , mkItem "shoot/Crossbow, light" "}:1d8:5:Ammo"          pos
       , mkItem "shoot/Crossbow, hand"  "}:1d6:3:Ammo, Light"   pos
       , mkItem "shoot/Crossbow, heavy" "}:1d10:18:Ammo, Heavy" pos
       , mkItem "shoot/Dart"            "}:1d4:1:Ammo"          pos
       , mkItem "shoot/Javelin"         "|:1d6:2:Ammo"          pos
       , mkItem "shoot/Longbow"         "}:1d8:2:Ammo, Heavy"   pos
       , mkItem "shoot/Sling"           "}:1d4:0:Ammo"          pos
       , mkItem "shoot/Shortbow"        "}:1d6:2:Ammo"          pos
       , mkItem "shoot/Throwing Knife"  "|:1d4:1:Ammo"          pos
       , mkItem "shoot/Throwing Axe"    "|:1d6:2:Ammo"          pos
       -- shoot Elvish
       , mkItem "shoot/Elvish Bow"      "}:2d4:2:Ammo, Magic"   pos
       , mkItem "shoot/Elvish Longbow"  "}:2d5:2:Ammo, Magic"   pos
       , mkItem "shoot/Elvish Shortbow" "}:2d3:2:Ammo, Magic"   pos
       -- armor
       , mkItem "armor/Padded"          "[:11:8:Light Armor"   pos
       , mkItem "armor/Leather"         "[:11:10:Light Armor"  pos
       , mkItem "armor/Studded Leather" "[:12:13:Light Armor"  pos
       , mkItem "armor/Hide"            "[:12:12:Medium Armor" pos
       , mkItem "armor/Chain Shirt"     "[:13:20:Medium Armor" pos
       , mkItem "armor/Scale Mail"      "[:14:45:Medium Armor" pos
       , mkItem "armor/Breastplate"     "[:14:20:Medium Armor" pos
       , mkItem "armor/Half Plate"      "[:15:40:Medium Armor" pos
       , mkItem "armor/Ring Mail"       "[:14:40:Heavy Armor"  pos
       , mkItem "armor/Chain Mail"      "[:16:55:Heavy Armor"  pos
       , mkItem "armor/Splint"          "[:17:60:Heavy Armor"  pos
       , mkItem "armor/Plate"           "[:18:65:Heavy Armor"  pos
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
       -- Rings
       , mkItem "jewelry/Ring"            "=:10:"  pos
       , mkItem "jewelry/Ring of Dwarves" "=:100:con+2" pos
       , mkItem "jewelry/Ring of Elves"   "=:100:dex+1,int+1" pos
       , mkItem "jewelry/Ring of Magi"    "=:100:int+2" pos
       , mkItem "jewelry/Ring of Priest"  "=:100:wis+2" pos
       , mkItem "jewelry/Ring of Thief"   "=:100:dex:+2" pos
       , mkItem "jewelry/Ring of West"    "=:100:str+1,dex+1,int+1,wis+1" pos
       , mkItem "jewelry/Ring of Warrior" "=:100:str+2" pos
       -- Amulets
       , mkItem "neck/Amulet"               "\":10:" pos
       , mkItem "neck/Amulet of Arms"       "\":100:str+2" pos
       , mkItem "neck/Amulet of Archery"    "\":100:dex+1,int+1" pos
       , mkItem "neck/Amulet of Dwarves"    "\":100:str+1,con+1" pos
       , mkItem "neck/Amulet of Elves"      "\":100:int+1,wis+1" pos
       , mkItem "neck/Amulet of Protection" "\":100:con+1,ac+1" pos
       , mkItem "neck/Amulet of Wizardry"   "\":100:int+2" pos
       , mkItem "neck/Amulet of Devotion"   "\":100:wis+2" pos
       -- Items
       , mkItem "cloak/Cloak"  "("  pos
       -- Monsters
       , mkMonster "Mouse"        "Small beast (r)"     pos
       , mkMonster "Orc"          "Medium humanoid (o)" pos
       , mkMonster "Orc Archer"   "Medium humanoid (o)" pos
       , mkMonster "Orc Shaman"   "Medium humanoid (o)" pos
       , mkMonster "Ogre"         "Large giant (O)"     pos
       , mkMonster "Spider"       "Large beast (S)"     pos
       , mkMonster "Troll"        "Large giant (T)"     pos
       , mkMonster "Wolf"         "Medium beast (c)"    pos
       , mkMonster "Dire Wolf"    "Large beast (C)"     pos
       , mkMonster "Red Dragon"   "Medium dragon (d)"   pos
       , mkMonster "Green Dragon" "Medium dragon (d)"   pos
       , mkMonster "Blue Dragon"  "Medium dragon (d)"   pos
       , mkMonster "Black Dragon" "Medium dragon (d)"   pos
       , mkMonster "White Dragon" "Medium dragon (d)"   pos
       , mkMonster "Wyvern"       "Large dragon (D)"    pos
       ]
  in Map.fromList $ zip [0..] em
