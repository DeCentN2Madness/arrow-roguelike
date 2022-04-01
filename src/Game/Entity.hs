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
                   , insertEntity
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

-- | insertEntity
insertEntity :: Int -> Coord -> Entity -> EntityMap -> EntityMap
insertEntity ix xy ek em = let
  e = mkEntity ek xy
  in Map.insert ix e em

-- | mkAssetMap
-- All the assets within the World
mkAssetMap :: [EntityKind] -> AssetMap
mkAssetMap ek = let
  spawn = (0, 0)
  em = if null ek
    then [ mkEntity Actor spawn
         , mkEntity Coin spawn
         , mkEntity Corpse spawn
         , mkEntity Item spawn
         , mkEntity Mushroom spawn
         , mkEntity Potion spawn
         , mkEntity StairDown spawn
         , mkEntity StairUp spawn
         , mkEntity Trap spawn
         , mkEntity Arrow spawn
         , mkMonster "Cleric"  "Medium human (h)" spawn
         , mkMonster "Fighter" "Medium human (h)" spawn
         , mkMonster "Ranger"  "Medium human (h)" spawn
         , mkMonster "Rogue"   "Medium human (h)" spawn
         , mkMonster "Wizard"  "Medium human (h)" spawn
         , mkMonster "Mouse"   "Small beast (r)" spawn
         , mkMonster "Orc"     "Medium humanoid (o)" spawn
         , mkMonster "Spider"  "Large beast (S)" spawn
         , mkMonster "Troll"   "Large giant (T)" spawn
         , mkMonster "Wolf"    "Medium beast (c)" spawn
         , mkMonster "Dragon"  "Medium dragon (d)" spawn
         ]
    else ek
  in Map.fromList $ zip [0..] em

-- | mkEntityMap will do more
-- insert Monsters
-- insert the Hero at 0
mkEntityMap :: Depth -> TileMap -> AssetMap -> EntityMap
mkEntityMap depth tm am = let
  player   = mkEntity Actor (2,2)
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
    then mkEntity Actor (2,2)
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
