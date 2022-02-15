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
insertRand :: Entity -> Int -> Int -> [Coord] -> [(Int, EntityKind)]
insertRand e start end openList = let
  sz = length openList - 1
  randList = DS.rollList (end-start) (fromIntegral sz) (end*sz)
  entityList = [ ek | ix <- randList, let ek = mkEntity e (openList!!ix) ]
  in zip [start..end] entityList

-- | mkAssetMap
-- All the entities within the World
mkAssetMap :: [EntityKind] -> EntityMap
mkAssetMap ek = let
  spawn = (0, 0)
  em = if null ek
    then [ mkEntity Actor spawn
         , mkEntity Coin spawn
         , mkEntity Corpse spawn
         , mkEntity Item spawn
         , mkEntity Mouse spawn
         , mkEntity Mushroom spawn
         , mkEntity Potion spawn
         , mkEntity StairDown spawn
         , mkEntity StairUp spawn
         , mkEntity Trap spawn
         , mkEntity Unknown spawn
         ]
    else ek
  in Map.fromList $ zip [0..] em

-- | mkEntityMap will do more
-- insert % of many things
-- preserve 0 for the Hero
mkEntityMap :: TileMap -> EntityMap
mkEntityMap tm = let
  openList = tail $ [ pos | (_, pos) <- GT.fromOpen tm ]
  p = mkEntity Actor (0, 0)
  junk = concat [ insertRand Mouse    1  10 openList
                , insertRand Mushroom 11 20 openList
                , insertRand Corpse   21 30 openList
                , insertRand Potion   31 40 openList
                , insertRand Coin     41 50 openList
                , insertRand Unknown  51 60 openList
                ]
  in safeInsertEntity 0 p tm (Map.fromList junk)

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
