{-# LANGUAGE OverloadedStrings #-}
{-

Game.Entity.hs

Game.Entity is the engine for the EntityKind.

Usage:
  mkAssetMap is all the Entity in the World
  mkEntityMap is all the Entity within a World Level

Example:
  getEntityAt uses the EntityMap by Ix
  getEntityBy uses the EntityMap by Coord

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Entity (EntityMap
                   , fromBlock
                   , fromEntityAt
                   , fromEntityStack
                   , getEntityAt
                   , getEntityBy
                   , mkAssetMap
                   , mkEntityMap
                   , safeInsertEntity
                   , updateEntity
                   , updateEntityHp
                   , updateEntityPos) where

import Prelude hiding (lookup)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Game.Kind.Entity
import Game.Kind.Monster
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type AssetMap = EntityMap
type Coord = (Int, Int)
type Depth = Int
type EntityMap = Map Int EntityKind

-- | fromBlock
fromBlock :: EntityMap -> [(Int, Coord)]
fromBlock em = [ (ix, xy) | (ek, ix) <- filter (\(i, _) -> block i) $
                 fromEntityAt em, let xy = coord ek ]

-- | fromEntityAt ix
fromEntityAt :: EntityMap -> [(EntityKind, Int)]
fromEntityAt em = [ (ek, ix) | (ix, ek) <- Map.toList em ]

-- | fromEntityStack
-- single entity per Coord for Engine.Draw.Visual
fromEntityStack :: EntityMap -> [(EntityKind, Coord)]
fromEntityStack em = let
  entityT = listToMap $ [ (ek, xy) | (_, ek) <- Map.toList em,
                          let xy = coord ek ]
  entityList = [ (ek, xy) | (xy, e) <- Map.toList entityT,
                 let ek = minimumBy (comparing kind) e ]
  in entityList

-- | listToMap
listToMap :: [(EntityKind, Coord)] -> Map Coord [EntityKind]
listToMap []         = Map.empty
listToMap ((v,k):xs) = Map.insertWith (++) k [v] (listToMap xs)

-- | getEntityAt ix
getEntityAt :: Int -> EntityMap -> (EntityKind, Coord)
getEntityAt ix em = let
  (Just ek) = Map.lookup ix em
  in (ek, coord ek)

-- | getEntityBy Coord
getEntityBy :: Coord -> EntityMap -> [(EntityKind, Coord)]
getEntityBy pos em = let
  entityList = [(ix, xy) | (ix, ek) <- Map.toList em, let xy = coord ek ]
  in [ e | (ix, _) <- filter ((==pos).snd) entityList,
       let e = getEntityAt ix em ]

-- | mkAssetMap
-- All the assets within the World by Ix
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
       , mkItem "melee/Dagger"        "|" pos
       , mkItem "melee/Sword"         "|" pos
       , mkItem "melee/Axe"           "|" pos
       , mkItem "melee/Spear"         "|" pos
       , mkItem "melee/Warhammer"     "|" pos
       , mkItem "melee/Zweihander"    "|" pos
       -- shield
       , mkItem "shield/Buckler"      ")" pos
       , mkItem "shield/Shield"       ")" pos
       , mkItem "shield/Kite Shield"  ")" pos
       -- shoot
       , mkItem "shoot/Bow"           "}" pos
       , mkItem "shoot/Longbow"       "}" pos
       , mkItem "shoot/Shortbow"      "}" pos
       -- armor
       , mkItem "armor/Leather Jack"    "[" pos
       , mkItem "armor/Leather Jerkin"  "[" pos
       , mkItem "armor/Mail Coat"       "[" pos
       , mkItem "armor/Mail Shirt"      "[" pos
       , mkItem "armor/Breastplate"     "[" pos
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
       , mkItem "hands/Gloves"    "]" pos
       , mkItem "hands/Bracers"   "]" pos
       , mkItem "hands/Gauntlets" "]" pos
       -- Items
       , mkItem "jewelry/Ring" "=" pos
       , mkItem "neck/Amulet"  "\"" pos
       , mkItem "cloak/Cloak"  "(" pos
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
       , mkMonster "Dragon"       "Medium dragon (d)" pos
       , mkMonster "Red Dragon"   "Medium dragon (d)" pos
       , mkMonster "Green Dragon" "Medium dragon (d)" pos
       , mkMonster "Blue Dragon"  "Medium dragon (d)" pos
       , mkMonster "Black Dragon" "Medium dragon (d)" pos
       , mkMonster "White Dragon" "Medium dragon (d)" pos
       ]
  in Map.fromList $ zip [0..] em

-- | mkEntityMap will do more
-- insert Monsters
-- insert the Hero at 0
mkEntityMap :: Depth -> TileMap -> AssetMap -> EntityMap
mkEntityMap depth tm am = let
  player   = mkMonster "Player" "The Hero (@)" (0,0)
  monsters = mkMonsterMap depth tm am
  in safeInsertEntity 0 player tm monsters

-- | insert @ into the TileMap
safeInsertEntity :: Int -> EntityKind -> TileMap -> EntityMap -> EntityMap
safeInsertEntity ix ek tm em = let
  -- enter from stairs
  openList = sort $ [ pos | (_, pos) <- GT.fromOpen tm]
  xy = if coord ek `elem` openList
    then coord ek
    else head openList
  -- dead @... start in 1st vault
  newEntity = if kind ek == Corpse
    then mkMonster "Player" "The Hero (@)" (17,4)
    else ek
  in Map.insert ix (newEntity { coord = xy }) em

-- | updateEntity
updateEntity :: Int -> EntityKind -> EntityMap -> EntityMap
updateEntity = Map.insert

-- | updateEntity at ix
updateEntityHp :: Int -> Int -> EntityMap -> EntityMap
updateEntityHp ix hp em = let
  (Just ek) = Map.lookup ix em
  in Map.insert ix (ek { eHP = hp }) em

-- | updateEntity at ix
updateEntityPos :: Int -> Coord -> EntityMap -> EntityMap
updateEntityPos ix pos em = let
  (Just ek) = Map.lookup ix em
  in Map.insert ix (ek { coord = pos }) em
