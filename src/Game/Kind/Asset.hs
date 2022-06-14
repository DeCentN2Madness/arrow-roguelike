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

type AssetMap = Map Int EntityKind

-- | mkAssetMap
-- All the assets within the World by Ix
--   1. Melee Basic and Martial
--      a. glyph : Damage : Weight : Properties
--   2. Armor and Shields
--      a. glyph : AC : Weight : Properties
--   3. Head, Hands, and Feet
--      a. glyph : Rare : Weight : Properties
--   4. Rings, and Amulets
--      a. glyph : Rare : Weight: Properties
--   5. Cloaks
--      a. glyph : Rare : Weight: Properties
--   6. Monsters
--      a. Description
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
       , mkItem "melee/Club"            "|:1d4:2:Light"         pos
       , mkItem "melee/Dagger"          "|:1d4:1:Finesse,Light" pos
       , mkItem "melee/Greatclub"       "|:1d8:10:Heavy"        pos
       , mkItem "melee/Handaxe"         "|:1d6:2:Light"         pos
       , mkItem "melee/Light Hammer"    "|:1d4:2:"              pos
       , mkItem "melee/Mace"            "|:1d6:4:"              pos
       , mkItem "melee/Quarterstaff"    "|:1d6:4:"              pos
       , mkItem "melee/Sickle"          "|:1d4:2:Light"         pos
       , mkItem "melee/Spear"           "|:1d6:3:"              pos
       -- melee martial
       , mkItem "melee/Battleaxe"       "|:1d8:4:"              pos
       , mkItem "melee/Flail"           "|:1d8:2:"              pos
       , mkItem "melee/Glaive"          "|:1d10:6:Heavy"        pos
       , mkItem "melee/Greataxe"        "|:1d12:7:Heavy"        pos
       , mkItem "melee/Halberd"         "|:1d10:6:Heavy"        pos
       , mkItem "melee/Lance"           "|:1d12:6:Heavy"        pos
       , mkItem "melee/Longsword"       "|:1d8:3:"              pos
       , mkItem "melee/Maul"            "|:1d12:10:Heavy"       pos
       , mkItem "melee/Morningstar"     "|:1d8:4:"              pos
       , mkItem "melee/Military Flail"  "|:2d4:3:"              pos
       , mkItem "melee/Pike"            "|:1d10:18:Heavy"       pos
       , mkItem "melee/Rapier"          "|:1d8:2:Finesse"       pos
       , mkItem "melee/Scimitar"        "|:1d6:2:Finesse,Light" pos
       , mkItem "melee/Shortsword"      "|:1d6:2:Finesse,Light" pos
       , mkItem "melee/Trident"         "|:1d6:4:"              pos
       , mkItem "melee/Warhammer"       "|:1d12:6:Heavy"        pos
       , mkItem "melee/Zweihander"      "|:2d6:6:Heavy"         pos
       -- melee Elvish
       , mkItem "melee/Elvish Dagger"   "|:2d2:1:Magic" pos
       , mkItem "melee/Elvish Spear"    "|:2d3:3:Magic" pos
       , mkItem "melee/Elvish Sword"    "|:2d4:3:Magic" pos
       , mkItem "melee/Wizard Staff"    "|:2d5:4:Magic" pos
       -- shield
       , mkItem "shield/Buckler"        "):1:2:Shield" pos
       , mkItem "shield/Shield"         "):2:6:Shield" pos
       , mkItem "shield/Kite Shield"    "):2:5:Shield" pos
       -- shoot
       , mkItem "shoot/Blowgun"         "}:1d1:1:Ammo"         pos
       , mkItem "shoot/Crossbow, light" "}:1d8:5:Ammo"         pos
       , mkItem "shoot/Crossbow, hand"  "}:1d6:3:Ammo,Light"   pos
       , mkItem "shoot/Crossbow, heavy" "}:1d10:18:Ammo,Heavy" pos
       , mkItem "shoot/Dart"            "}:1d4:1:Thrown"       pos
       , mkItem "shoot/Javelin"         "|:1d6:2:Thrown"       pos
       , mkItem "shoot/Longbow"         "}:1d8:2:Ammo,Heavy"   pos
       , mkItem "shoot/Sling"           "}:1d4:0:Ammo"         pos
       , mkItem "shoot/Shortbow"        "}:1d6:2:Ammo"         pos
       , mkItem "shoot/Throwing Knife"  "|:1d4:1:Thrown"       pos
       , mkItem "shoot/Throwing Axe"    "|:1d6:2:Thrown"       pos
       -- shoot Elvish
       , mkItem "shoot/Elvish Longbow"  "}:2d5:2:Ammo" pos
       , mkItem "shoot/Elvish Shortbow" "}:2d3:2:Ammo" pos
       -- Armor
       , mkItem "armor/Padded"          "[:11:8:Light_Armor"   pos
       , mkItem "armor/Leather"         "[:11:10:Light_Armor"  pos
       , mkItem "armor/Studded Leather" "[:12:13:Light_Armor"  pos
       , mkItem "armor/Hide"            "[:12:12:Medium_Armor" pos
       , mkItem "armor/Chain Shirt"     "[:13:20:Medium_Armor" pos
       , mkItem "armor/Scale Mail"      "[:14:45:Medium_Armor" pos
       , mkItem "armor/Breastplate"     "[:14:20:Medium_Armor" pos
       , mkItem "armor/Half Plate"      "[:15:40:Medium_Armor" pos
       , mkItem "armor/Ring Mail"       "[:14:40:Heavy_Armor"  pos
       , mkItem "armor/Chain Mail"      "[:16:55:Heavy_Armor"  pos
       , mkItem "armor/Splint"          "[:17:60:Heavy_Armor"  pos
       , mkItem "armor/Plate"           "[:18:65:Heavy_Armor"  pos
       -- Head
       , mkItem "head/Leather Skullcap"  "]:1:1:" pos
       , mkItem "head/Crown of Elements" "]:100:1:FIRE,COLD" pos
       , mkItem "head/Crown of King"     "]:100:1:WIS,FIRE,COLD,SEARCH" pos
       , mkItem "head/Mail Coif"         "]:100:1:AC" pos
       , mkItem "head/Open Helm"         "]:100:1:AC" pos
       , mkItem "head/Helm"              "]:100:1:AC" pos
       , mkItem "head/Jousting Helm"     "]:100:1:AC,CON" pos
       , mkItem "head/Crown of Elves"    "]:100:1:INT,FIRE,COLD,SEARCH" pos
       -- Feet
       , mkItem "feet/Boots"              "]:1:1:" pos
       , mkItem "feet/Boots of Elvenkind" "]:100:1:DEX,COLD" pos
       , mkItem "feet/Boots of Speed"     "]:100:1:DEX,AC" pos
       , mkItem "feet/Dwarven Boots"      "]:100:1:STR,COLD" pos
       , mkItem "feet/Horseman Boots"     "]:100:1:CON,FIRE" pos
       , mkItem "feet/Mail Chausses"      "]:100:1:AC" pos
       , mkItem "feet/Plate Leggings"     "]:100:1:AC" pos
       , mkItem "feet/Traveler Boots"     "]:100:1:COLD" pos
       -- Hands
       , mkItem "hands/Gloves"              "]:1:1:" pos
       , mkItem "hands/Gloves of Thieves"   "]:100:1:DEX,INT" pos
       , mkItem "hands/Bracers of Warrior"  "]:100:1:STR,ATTACK" pos
       , mkItem "hands/Bracers of Hunter"   "]:100:1:DEX,SHOOT" pos
       , mkItem "hands/Gauntlets of Ogre"   "]:100:1:STR,CON" pos
       , mkItem "hands/Gauntlets of Power"  "]:100:1:INT,CON" pos
       , mkItem "hands/Gauntlets of Frost"  "]:100:1:COLD" pos
       , mkItem "hands/Gauntlets of Flames" "]:100:1:FIRE" pos
       -- Rings
       , mkItem "jewelry/Ring"            "=:10:1:" pos
       , mkItem "jewelry/Ring of Dwarves" "=:100:1:CON" pos
       , mkItem "jewelry/Ring of Elves"   "=:100:1:INT,WIS" pos
       , mkItem "jewelry/Ring of Magi"    "=:100:1:INT" pos
       , mkItem "jewelry/Ring of Priest"  "=:100:1:WIS" pos
       , mkItem "jewelry/Ring of Thief"   "=:100:1:DEX" pos
       , mkItem "jewelry/Ring of West"    "=:100:1:STR,DEX,CON,INT,WIS" pos
       , mkItem "jewelry/Ring of Warrior" "=:100:1:STR" pos
       -- Amulets
       , mkItem "neck/Amulet"               "\":10:1:" pos
       , mkItem "neck/Amulet of Archery"    "\":100:1:DEX,CON" pos
       , mkItem "neck/Amulet of Arms"       "\":100:1:STR,CON" pos
       , mkItem "neck/Amulet of Devotion"   "\":100:1:STR,WIS" pos
       , mkItem "neck/Amulet of Dwarves"    "\":100:1:STR,CON" pos
       , mkItem "neck/Amulet of Elves"      "\":100:1:INT,WIS" pos
       , mkItem "neck/Amulet of Protection" "\":100:1:AC" pos
       , mkItem "neck/Amulet of Wizardry"   "\":100:1:INT,WIS" pos
       -- Cloaks
       , mkItem "cloak/Cloak"                "(:1:1:" pos
       , mkItem "cloak/Cloak of Iron Throne" "(:100:1:CON,SEARCH" pos
       , mkItem "cloak/Cloak of Elvenkind"   "(:100:1:DEX,SEARCH" pos
       , mkItem "cloak/Cloak of Elements"    "(:100:1:FIRE,COLD" pos
       , mkItem "cloak/Cloak of Protection"  "(:100:1:AC" pos
       , mkItem "cloak/Cloak of Dragon"      "(:100:1:CON,FIRE" pos
       , mkItem "cloak/Cloak of Hunter"      "(:100:1:DEX,SHOOT" pos
       , mkItem "cloak/Cloak of Winter"      "(:100:1:CON,COLD" pos
       -- Monsters
       , mkMonster "Mouse"        "Small beast (r)"     pos
       , mkMonster "3 Hydra"      "Large monster (M)"   pos
       , mkMonster "Goblin"       "Small humanoid (o)"  pos
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
