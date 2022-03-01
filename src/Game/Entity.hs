{-

Game.Entity.hs

Game.Entity is the engine for the EntityKind.

Example:
  getEntityAt uses the EntityMap by Ix
  getEntityBy uses the EntityMap by Coord

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Entity (EntityMap
               , fromBlock
               , fromEntityAt
               , fromEntityBy
               , getEntityAt
               , getEntityBy
               , insertEntity
               , mkAssetMap
               , mkEntityMap
               , safeInsertEntity
               , updateEntityHp
               , updateEntityPos) where

import Prelude hiding (lookup)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Game.DiceSet as DS
import Game.Kind.Entity
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type Coord = (Int, Int)
type Depth = Int
type EntityMap = Map Int EntityKind
type AssetMap = EntityMap
type NameMap = Map String EntityKind

-- | fromBlock
fromBlock :: EntityMap -> [(Int, Coord)]
fromBlock em = let
  entityList = [ (ix, xy) | (ek, ix) <- filter (\(i, _) -> block i) $
                 fromEntityAt em,
                 let xy = coord ek ]
  in entityList

-- | fromEntityAt ix
fromEntityAt :: EntityMap -> [(EntityKind, Int)]
fromEntityAt em = let
  entityList = [ (ek, ix) | (ix, ek) <- Map.toList em ]
  in entityList

-- | fromEntityBy Coord
fromEntityBy :: EntityMap -> [(EntityKind, Coord)]
fromEntityBy em = let
  entityList = [ (ek, xy) | (_, ek) <- Map.toList em, let xy = coord ek ]
  in entityList

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

-- | inserEntity
insertEntity :: Int -> Coord -> Entity -> EntityMap -> EntityMap
insertEntity ix xy ek em = let
  e = mkEntity ek xy
  in Map.insert ix e em

-- | insertRand all over the TileMap
insertRand :: EntityKind -> Int -> Int -> [Coord] -> [(Int, EntityKind)]
insertRand ek start end openList = let
  sz = length openList - 1
  randList = DS.rollList (end-start) (fromIntegral sz) (end*sz)
  ePos i = openList!!i
  entityList = [ e | ix <- randList, let e = updateEntitySpawn ek (ePos ix) ]
  in zip [start..end] entityList

-- | mkNameMap
-- All the assets by Name
mkNameMap :: AssetMap -> NameMap
mkNameMap am = let
  assetList = [ (name, ek) | (_, ek) <- Map.toList am,
                let eProp = property ek
                    name  = Map.findWithDefault "0" "Name" eProp ]
  in Map.fromList assetList

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
         , mkEntity Unknown spawn
         , mkMonster "Mouse"  "Small beast (r)" spawn
         , mkMonster "Orc"    "Medium humanoid (o)" spawn
         , mkMonster "Spider" "Large beast (S)" spawn
         , mkMonster "Troll"  "Large giant (T)" spawn
         , mkMonster "Wolf"   "Medium beast (c)" spawn
         , mkMonster "Dragon" "Medium dragon (d)" spawn
         ]
    else ek
  in Map.fromList $ zip [0..] em

-- | mkEntityMap will do more
-- insert many things
-- insert Monsters
-- insert the Hero at 0
mkEntityMap :: Depth -> TileMap -> AssetMap -> EntityMap
mkEntityMap depth tm am = let
  openList = tail $ sort $ [ pos | (_, pos) <- GT.fromOpen tm ]
  topRight = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (20, 1) then pos else (0,0) ]
  bottomLeft = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (20, 20) then pos else (0,0) ]
  bottomRight = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (40, 20) then pos else (0,0) ]
  unk     = mkEntity Unknown (0, 0)
  assets  = mkNameMap am
  player  = Map.findWithDefault unk "Player"   assets
  shrooms = Map.findWithDefault unk "Mushroom" assets
  corpses = Map.findWithDefault unk "Corpse"   assets
  potions = Map.findWithDefault unk "Potion"   assets
  coins   = Map.findWithDefault unk "Coin"     assets
  unknown = Map.findWithDefault unk "Unknown"  assets
  mice    = Map.findWithDefault unk "Mouse"    assets
  -- add powerful monsters deeper to level...
  wolves  = Map.findWithDefault unk "Wolf"    assets
  orcs    = Map.findWithDefault unk "Orc"     assets
  dragons = Map.findWithDefault unk "Dragon"  assets
  spiders = Map.findWithDefault unk "Spider"  assets
  trolls  = Map.findWithDefault unk "Troll"   assets
  -- fill the dungeon...
  junk
    | depth >= 15 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand unknown 41 50 openList
             , insertRand mice    51 60 openList
             , insertRand wolves  61 70 topRight
             , insertRand orcs    70 80 topRight
             , insertRand spiders 80 85 bottomLeft
             , insertRand dragons 85 90 bottomLeft
             , insertRand trolls  90 95 bottomRight
                ]
    | depth >= 10 && depth < 15 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand unknown 41 50 openList
             , insertRand mice    51 60 openList
             , insertRand wolves  61 70 topRight
             , insertRand orcs    70 80 bottomLeft
             , insertRand spiders 80 85 bottomRight
             ]
    | depth >= 5 && depth < 10 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand unknown 41 50 openList
             , insertRand mice    51 60 openList
             , insertRand wolves  61 70 topRight
             ]
   | otherwise =
     concat [ insertRand shrooms 1  10 openList
            , insertRand corpses 11 20 openList
            , insertRand potions 21 30 openList
            , insertRand coins   31 40 openList
            , insertRand unknown 41 50 openList
            , insertRand mice    51 60 openList
            ]
  in safeInsertEntity 0 player tm (Map.fromList junk)

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

-- | updateEntitySpawn
updateEntitySpawn :: EntityKind -> Coord -> EntityKind
updateEntitySpawn ek pos = let
  eProp = property ek
  newProp = Map.insert "spawn" (show pos) eProp
  in ek { coord = pos, property = newProp }
