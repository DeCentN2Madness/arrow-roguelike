{-

Game.Entity.hs

Game.Entity is the engine for the EntityKind.

Example:
  getEntityAt uses the EntityMap by index Ix
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
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Game.DiceSet as DS
import Game.Tile (TileMap)
import qualified Game.Tile as GT
import Game.Kind.Entity

type Coord = (Int, Int)
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
         , mkMonster "Orc" "Medium humanoid (o)" spawn
         , mkMonster "Mouse" "Small beast (r)" spawn
         , mkMonster "Spider" "Large beast (S)" spawn
         ]
    else ek
  in Map.fromList $ zip [0..] em

-- | mkEntityMap will do more
-- insert % of many things
-- preserve 0 for the Hero
mkEntityMap :: TileMap -> AssetMap -> EntityMap
mkEntityMap tm am = let
  openList = tail $ [ pos | (_, pos) <- GT.fromOpen tm ]
  unk      = mkEntity Unknown (0, 0)
  assetMap = mkNameMap am
  p0 = Map.findWithDefault unk "Player" assetMap
  e0 = Map.findWithDefault unk "Mouse" assetMap
  e1 = Map.findWithDefault unk "Mushroom" assetMap
  e2 = Map.findWithDefault unk "Corpse" assetMap
  e3 = Map.findWithDefault unk "Potion" assetMap
  e4 = Map.findWithDefault unk "Coin" assetMap
  e5 = Map.findWithDefault unk "Unknown" assetMap
  e6 = Map.findWithDefault unk "Orc" assetMap
  e7 = Map.findWithDefault unk "Spider" assetMap
  junk = concat [ insertRand e0 1  10 openList
                , insertRand e1 11 20 openList
                , insertRand e2 21 30 openList
                , insertRand e3 31 40 openList
                , insertRand e4 41 50 openList
                , insertRand e5 51 60 openList
                , insertRand e6 61 63 openList
                , insertRand e7 63 65 openList ]
  in safeInsertEntity 0 p0 tm (Map.fromList junk)

-- | insert @ into the TileMap
safeInsertEntity :: Int -> EntityKind -> TileMap -> EntityMap -> EntityMap
safeInsertEntity ix ek tm em = let
  openList = [ pos | (_, pos) <- GT.fromOpen tm]
  xy = if coord ek `elem` openList
    then coord ek
    else head openList
  -- dead Player
  newEntity = if kind ek == Corpse
    then mkEntity Actor xy
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
-- TODO eLvl, eHP, eMaxHP, eXP, and so on
updateEntitySpawn :: EntityKind -> Coord -> EntityKind
updateEntitySpawn ek pos = let
  eProp = property ek
  newProp = Map.insert "spawn" (show pos) eProp
  in ek { coord = pos, property = newProp }
