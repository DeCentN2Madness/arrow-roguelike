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
               , insertMouse
               , insertPlayer
               , mkEntityMap
               , updatePlayer) where

import Control.Monad.Random (StdGen)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
getPlayer :: EntityMap -> Coord
getPlayer em = let
  e = getEntityAt 0 em
  in coord e

getEntityAt :: Int -> EntityMap -> EntityKind
getEntityAt xy em = let
  (Just e) = Map.lookup xy em
  in e

insertEntity :: Int -> Coord -> Entity -> StdGen -> EntityMap -> EntityMap
insertEntity k xy ek g em = let
  e = mkEntity ek xy g
  in Map.insert k e em

-- | insert @ into the TileMap
insertPlayer :: TileMap -> EntityMap -> StdGen -> EntityMap
insertPlayer tm em g = let
  openList = [ v | (_, v) <- fromOpen tm]
  xy = head openList
  in insertEntity 0 xy Actor g em

-- | three blind mice
insertMouse :: TileMap -> StdGen -> EntityMap
insertMouse tm g = let
  openList = tail $ [ xy | (_, xy) <- fromOpen tm ]
  miceList = [ m | v <- openList, let m = mkEntity Mouse v g ]
  mice = zip [1..3] miceList
  in insertPlayer tm (Map.fromList mice) g

-- | mkEntityMap will do more
mkEntityMap :: TileMap -> StdGen -> EntityMap
mkEntityMap = insertMouse

-- update @ position
updatePlayer :: Coord -> EntityMap -> EntityMap
updatePlayer v em = let
  (Just e) = Map.lookup 0 em
  in Map.insert 0 (e { coord = v}) em
