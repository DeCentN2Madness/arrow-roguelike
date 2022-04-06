{-# LANGUAGE OverloadedStrings #-}
{-

Game.Inventory.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Inventory (emptyBy
                      , mkItem
                      , pickUp
                      , pickList
                      , putDown) where

import Prelude hiding (lookup)
import Control.Arrow ((&&&))
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Entity (EntityMap)
import Game.Kind.Entity

type AssetMap = EntityMap
type Coord = (Int, Int)
type Inventory = Map Text Int
type NameMap = Map Text EntityKind

-- | encumberance
-- Twenty is the limit, except Coin...
encumberance :: Inventory -> Inventory
encumberance inv = let
  invT = [ (k, v) | (k, j) <- Map.toList inv,
           let v = case k of
                 "Coin" -> j
                 _      -> if j > 20 then 20 else j ]
  in Map.fromList invT

-- | emptyBy
-- Remove the picked items from the map...
emptyBy :: Coord -> [(EntityKind, Coord)] -> EntityMap -> EntityMap
emptyBy pos items em = let
  entityIX = [ (xy, ix, e) | (ix, e) <- Map.toList em, let xy = coord e ]
  deleteList = [ d | (xy, i, e) <- entityIX,
                 let d = if xy == pos &&
                       kind e `elem` pickList items
                       then i else (-1) ]
  newEntity = filter ((/=(-1)).fst) $
    [ (ix, ek) | (i, ek) <- Map.toList em,
      let ix = if i `notElem` deleteList then i else (-1) ]
  in if not (null deleteList)
  then Map.fromList newEntity
  else em

-- | groupEK builds counts from a list
groupEK :: [String] -> [(String, Int)]
groupEK = map (head &&& length) . group . sort

-- | mkNameMap
-- All the assets by Name
mkNameMap :: AssetMap -> NameMap
mkNameMap am = let
  assetList = [ (name, ek) | (_, ek) <- Map.toList am,
                let name = Map.findWithDefault "M" "Name" (property ek) ]
  in Map.fromList assetList

-- | mkItem
mkItem :: Text -> Coord -> AssetMap -> EntityKind
mkItem name pos am = let
  assets = mkNameMap am
  coin   = mkEntity Coin pos
  item   = Map.findWithDefault coin name assets
  in item { coord = pos, spawn = pos }

-- | noPickup
noPickup :: [Entity]
noPickup = [Actor, Corpse, Monster, StairDown, StairUp, Trap]

-- | pickList
pickList :: [(EntityKind, Coord)] -> [Entity]
pickList items = filter (`notElem` noPickup) $ [ kind k | (k, _) <- items ]

-- | pickUp
pickUp :: [(EntityKind, Coord)] -> EntityKind -> EntityKind
pickUp items ek = let
  invT  = groupEK $ map show (pickList items)
  invM  = Map.fromList $ [ (T.pack k, v) | (k, v) <- invT ]
  picks = Map.unionWith (+) (inventory ek) invM
  in ek { inventory = encumberance picks }

-- | putDown
putDown :: EntityKind -> EntityMap -> EntityMap
putDown item em = let
  ix = 1 + maximum (Map.keys em)
  in Map.insert ix item em
