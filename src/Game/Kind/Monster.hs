{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Monster.hs

Game.Kind.Monster adds Monsters to the Map

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Monster (mkMonsterMap, updateEntitySpawn) where

import Prelude hiding (lookup)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Game.DiceSet as DS
import Game.Kind.Entity
import Game.Tile (TileMap)
import qualified Game.Tile as GT

type AssetMap = EntityMap
type Coord = (Int, Int)
type Depth = Int
type EntityMap = Map Int EntityKind
type NameMap = Map Text EntityKind

-- | insertRand all over the TileMap
insertRand :: EntityKind -> Int -> Int -> [Coord] -> [(Int, EntityKind)]
insertRand ek start end openList = let
  sz = length openList - 1
  randList = DS.rollList (end-start) (fromIntegral sz) (end*sz)
  ePos i = nth i openList
  entityList = [ e | ix <- randList, let e = updateEntitySpawn ek (ePos ix) ]
  in zip [start..end] entityList

-- | mkMonsterMap
-- adds Monsters based on depth
mkMonsterMap :: Depth -> TileMap -> AssetMap -> EntityMap
mkMonsterMap depth tm am = let
  openList = tail $ sort $ [ pos | (_, pos) <- GT.fromOpen tm ]
  topRight = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (20, 1) then pos else (0,0) ]
  bottomLeft = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (20, 20) then pos else (0,0) ]
  bottomRight = filter (/=(0,0)) $
    [ xy | pos <- openList, let xy = if pos > (40, 20) then pos else (0,0) ]
  assets  = mkNameMap am
  arr     = mkItem "Arrow" "~" (0, 0)
  -- common to levels...
  arrows  = Map.findWithDefault arr "Arrow"    assets
  coins   = Map.findWithDefault arr "Coin"     assets
  corpses = Map.findWithDefault arr "Corpse"   assets
  mice    = Map.findWithDefault arr "Mouse"    assets
  potions = Map.findWithDefault arr "Potion"   assets
  shrooms = Map.findWithDefault arr "Mushroom" assets
  -- add powerful monsters deeper to level...
  wolves     = Map.findWithDefault arr "Wolf"         assets
  orc        = Map.findWithDefault arr "Orc"          assets
  orcArcher  = Map.findWithDefault arr "Orc Archer"   assets
  orcShaman  = Map.findWithDefault arr "Orc Shaman"   assets
  dragons    = Map.findWithDefault arr "Green Dragon" assets
  spiders    = Map.findWithDefault arr "Spider"       assets
  trolls     = Map.findWithDefault arr "Troll"        assets
  -- fill the dungeon...
  monsters
    | depth >= 18 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand arrows  41 50 openList
             , insertRand orc       51 60 openList
             , insertRand orcArcher 61 70 openList
             , insertRand orcShaman 71 80 topRight
             , insertRand spiders   81 90 topRight
             , insertRand dragons   91 100 bottomLeft
             , insertRand trolls    101 105 bottomRight
             ]
    | depth >= 15 && depth < 18 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand arrows  41 50 openList
             , insertRand orc       51 60 openList
             , insertRand orcArcher 61 70 topRight
             , insertRand orcShaman 71 80 bottomLeft
             , insertRand spiders   81 90 bottomLeft
             , insertRand dragons   91 100 bottomRight
             ]
    | depth >= 10 && depth < 15 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand arrows  41 50 openList
             , insertRand wolves  51 60 openList
             , insertRand orc       61 70 topRight
             , insertRand orcArcher 71 80 bottomLeft
             , insertRand orcShaman 81 90 bottomRight
             , insertRand spiders   91 95 bottomRight
             ]
    | depth >= 5 && depth < 10 =
      concat [ insertRand shrooms 1  10 openList
             , insertRand corpses 11 20 openList
             , insertRand potions 21 30 openList
             , insertRand coins   31 40 openList
             , insertRand arrows  41 50 openList
             , insertRand mice    51 60 openList
             , insertRand wolves  61 70 topRight
             ]
   | otherwise =
     concat [ insertRand shrooms 1  10 openList
            , insertRand corpses 11 20 openList
            , insertRand potions 21 30 openList
            , insertRand coins   31 40 openList
            , insertRand arrows  41 50 openList
            , insertRand mice    51 60 openList
            ]
  in Map.fromList monsters

-- | mkNameMap
-- All the assets by Name
mkNameMap :: AssetMap -> NameMap
mkNameMap am = let
  assetList = [ (name, ek) | (_, ek) <- Map.toList am,
                let eProp = property ek
                    name  = Map.findWithDefault "M" "Name" eProp ]
  in Map.fromList assetList

-- | nth safe chooser
nth :: Int -> [(Int, Int)] -> (Int, Int)
nth _ []     = (0, 0)
nth 1 (x:_)  = x
nth n (_:xs) = nth (n-1) xs

-- | updateEntitySpawn
updateEntitySpawn :: EntityKind -> Coord -> EntityKind
updateEntitySpawn ek pos = ek { coord = pos, spawn = pos }
