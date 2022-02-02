{-

Game.Actor.hs

Game.Actor is the engine for the Entity Kind.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Actor(EntityMap
               , fromBlock
               , fromEntity
               , getPlayer
               , getEntityAt
               , getEntityBy
               , insertPlayer
               , mkEntityMap
               , updateEntity
               , updatePlayer) where

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
  entityList = [ (i, xy) | (i, ek) <- Map.toList em,
                 let xy = if block ek then coord ek else (0,0) ]
  in entityList

-- | fromEntity in the World
fromEntity :: EntityMap -> [(Entity, Coord)]
fromEntity em = let
  entityList = [ (t, pos) | (_, ek) <- Map.toList em,
                 let pos = coord ek
                     t = eKind ek ]
  in entityList

-- | @ lives at 0
getPlayer :: EntityMap -> (Entity, Coord)
getPlayer em = let
  e = getEntityAt 0 em
  in (eKind e, coord e)

-- | getEntityAt ix
getEntityAt :: Int -> EntityMap -> EntityKind
getEntityAt ix em = let
  (Just e) = Map.lookup ix em
  in e

-- | getEntityBy Coord returns ix
getEntityBy :: Coord -> EntityMap -> [Int]
getEntityBy xy em = let
  entityList = [(i, pos) | (i, ek) <- Map.toList em,
                let pos = coord ek ]
  in [ ix | (ix, _) <- filter ((==xy).snd) entityList ]

-- | inserEntity
insertEntity :: Int -> Coord -> Entity -> EntityMap -> EntityMap
insertEntity ix xy ek em = let
  e = mkEntity ek xy
  in Map.insert ix e em

-- | insert @ into the TileMap
insertPlayer :: TileMap -> EntityMap -> EntityMap
insertPlayer tm em = let
  openList = [ v | (_, v) <- fromOpen tm]
  xy = head openList
  in insertEntity 0 xy Actor em

-- | insertRand all over the TileMap
insertRand :: Entity -> Int -> Int -> [Coord] -> [(Int, EntityKind)]
insertRand e start count openList = let
  end = start + count
  sz = length openList - 1
  randList = DS.rollList sz (fromIntegral sz) (end*sz)
  entityList = [ m | v <- randList, let m = mkEntity e (openList!!v) ]
  in zip [start..end] entityList

-- | mkEntityMap will do more
-- drop $ take
-- insert % of many things
-- preserve 0 for the Hero
mkEntityMap :: TileMap -> EntityMap
mkEntityMap tm = let
  openList = tail $ [ xy | (_, xy) <- fromOpen tm ]
  junk = concat [insertRand  Mouse    1  10 (drop 1  $ take 10 openList)
                , insertRand Mushroom 11 20 (drop 11 $ take 20 openList)
                , insertRand Corpse   21 30 (drop 21 $ take 30 openList)
                , insertRand Potion   31 40 (drop 31 $ take 40 openList)
                , insertRand Coin     41 50 (drop 41 $ take 50 openList)
                , insertRand Unknown  51 60 (drop 51 $ take 60 openList)
                ]
  in insertPlayer tm (Map.fromList junk)

-- | updateEntity at ix
updateEntity :: Int -> Int -> EntityMap -> EntityMap
updateEntity ix p em = let
  (Just e) = Map.lookup ix em
  in Map.insert ix (e { hitPoint = p }) em

-- | update @ position
updatePlayer :: Coord -> EntityMap -> EntityMap
updatePlayer v em = let
  (Just e) = Map.lookup 0 em
  in Map.insert 0 (e { coord = v }) em
