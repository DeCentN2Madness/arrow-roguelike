{-

Game.Actor.hs

Game.Actor is the engine for the EntityKind.

Example:
  getEntityAt uses the EntityMap by index Ix
  getEntityBy uses the EntityMap by Coord

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Actor(EntityMap
               , fromBlock
               , fromEntityAt
               , fromEntityBy
               , getPlayer
               , getEntityAt
               , getEntityBy
               , insertEntity
               , mkEntityMap
               , updateEntityHp
               , updatePlayerBy) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Game.DiceSet as DS
import Game.Tile (fromOpen, TileMap)
import Game.Kind.Entity

type Coord = (Int, Int)
type EntityMap = Map Int EntityKind

-- | fromBlock
fromBlock :: EntityMap -> [(Int, Coord)]
fromBlock em = let
  entityList = [ (ix, xy) | (ix, ek) <- Map.toList em,
                 let xy = if block ek then coord ek else (0,0) ]
  in entityList

-- | fromEntityAt ix
fromEntityAt :: EntityMap -> [(EntityKind, Int)]
fromEntityAt em = let
  entityList = [ (ek, ix) | (ix, ek) <- Map.toList em ]
  in entityList

-- | fromEntityBy Coord
fromEntityBy :: EntityMap -> [(EntityKind, Coord)]
fromEntityBy em = let
  entityList = [ (ek, pos) | (_, ek) <- Map.toList em,
                 let pos = coord ek ]
  in entityList

-- | getEntityAt ix
getEntityAt :: Int -> EntityMap -> (EntityKind, Coord)
getEntityAt ix em = let
  (Just e) = Map.lookup ix em
  in (e, coord e)

-- | getEntityBy Coord
getEntityBy :: Coord -> EntityMap -> [(EntityKind, Coord)]
getEntityBy xy em = let
  entityList = [(ix, pos) | (ix, ek) <- Map.toList em,
                let pos = coord ek ]
  in [ e | (ix, _) <- filter ((==xy).snd) entityList,
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
  entityList = [ ee | ix <- randList, let ee = mkEntity e (openList!!ix) ]
  in zip [start..end] entityList

-- | mkEntityMap will do more
-- insert % of many things
-- preserve 0 for the Hero
mkEntityMap :: TileMap -> EntityMap
mkEntityMap tm = let
  openList = tail $ [ xy | (_, xy) <- fromOpen tm ]
  junk = concat [insertRand  Mouse    1  20 openList
                , insertRand Mushroom 21 30 openList
                , insertRand Corpse   31 40 openList
                , insertRand Potion   41 50 openList
                , insertRand Coin     51 60 openList
                , insertRand Unknown  61 70 openList
                ]
  in insertPlayer tm (Map.fromList junk)

-- | updateEntity at ix
updateEntityHp :: Int -> Int -> EntityMap -> EntityMap
updateEntityHp ix hp em = let
  (Just e) = Map.lookup ix em
  in Map.insert ix (e { hitPoint = hp }) em

-- | updateEntity at ix
updateEntityPos :: Int -> Coord -> EntityMap -> EntityMap
updateEntityPos ix pos em = let
  (Just e) = Map.lookup ix em
  in Map.insert ix (e { coord = pos }) em

-- | @ lives at 0
-- get Player
getPlayer :: EntityMap -> (EntityKind, Coord)
getPlayer = getEntityAt 0

-- | insert @ into the TileMap
insertPlayer :: TileMap -> EntityMap -> EntityMap
insertPlayer tm em = let
  openList = [ v | (_, v) <- fromOpen tm]
  xy = head openList
  in insertEntity 0 xy Actor em

-- | update @ position
updatePlayerBy :: Coord -> EntityMap -> EntityMap
updatePlayerBy = updateEntityPos 0
