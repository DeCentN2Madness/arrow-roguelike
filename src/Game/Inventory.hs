{-# LANGUAGE OverloadedStrings #-}
{-

Game.Inventory.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Inventory (checkPickUp
                      , emptyBy
                      , mkDropItem
                      , mkRandItem
                      , pickUp
                      , pickList
                      , putDown) where

import Prelude hiding (lookup)
import Control.Arrow ((&&&))
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Game.DiceSet as DS
import Game.Entity (EntityMap)
import Game.Kind.Entity

type AssetMap = EntityMap
type Coord = (Int, Int)
type Inventory = Map Text Int
type NameMap = Map Text EntityKind

-- | checkPickUp
-- Did '@' actually PickUp?
checkPickUp :: Inventory -> Inventory -> Inventory
checkPickUp invA invB = let
  pickItems v1 v2 = if v1 /= v2 then Just v1 else Nothing
  in Map.differenceWith pickItems invA invB

-- | encumberance
-- Twenty is the limit, except Coin...
encumberance :: Inventory -> Inventory
encumberance inv = let
  invT = [ (k, v) | (k, j) <- Map.toList inv,
           let v = case k of
                 "Dagger" -> 1
                 "Bow"    -> 1
                 "Ring"   -> 1
                 "Amulet" -> 1
                 "Armor"  -> 1
                 "Cloak"  -> 1
                 "Shield" -> 1
                 "Helmet" -> 1
                 "Gloves" -> 1
                 "Boots"  -> 1
                 "Coin"   -> j
                 _        -> if j > 20 then 20 else j ]
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
groupEK :: [Text] -> [(Text, Int)]
groupEK = map (head &&& length) . group . sort

-- | mkNameMap
-- All the assets by Name
mkNameMap :: AssetMap -> NameMap
mkNameMap am = let
  assetList = [ (name, ek) | (_, ek) <- Map.toList am,
                let name = Map.findWithDefault "I" "Name" (property ek) ]
  in Map.fromList assetList

-- | mkDropItem
mkDropItem :: Text -> Coord -> AssetMap -> EntityKind
mkDropItem name pos am = let
  assets = mkNameMap am
  coin = mkItem "Coin" "$" pos
  item = Map.findWithDefault coin name assets
  in item { coord = pos, spawn = pos }

-- | random Item
mkRandItem :: Coord -> AssetMap -> EntityKind
mkRandItem pos am = let
  itemList = filter ((/=(-1)).fst) $ [ (ix, v) | (k, v) <- Map.toList am,
               let ix = if kind v == Item then k else (-1) ]
  seed = 1 + uncurry (*) pos
  sz = length itemList - 1
  itemRoll = head $ DS.rollList 1 (fromIntegral sz) seed
  item = nth itemRoll itemList
  in item { coord = pos, spawn = pos }

-- | nth safe chooser
nth :: Int -> [(Int, EntityKind)] -> EntityKind
nth _ []     = mkItem "Arrow" "~" (0, 0)
nth 1 (x:_)  = snd x
nth n (_:xs) = nth (n-1) xs

-- | noPickup
noPickup :: [Entity]
noPickup = [Actor, Corpse, Monster, StairDown, StairUp, Trap]

-- | pickList
pickList :: [(EntityKind, Coord)] -> [Entity]
pickList items = filter (`notElem` noPickup) $ [ kind k | (k, _) <- items ]

-- | pickUp
-- Items are "Text" keys in AssetMap
pickUp :: [(EntityKind, Coord)] -> EntityKind -> EntityKind
pickUp items ek = let
  invT = groupEK $ filter (/="I") $
    [ name | (e, _) <- items,
      let name = if kind e `elem` pickList items
            then Map.findWithDefault "I" "Name" (property e) else "I" ]
  picks = Map.unionWith (+) (inventory ek) (Map.fromList invT)
  in ek { inventory = encumberance picks }

-- | putDown
putDown :: EntityKind -> EntityMap -> EntityMap
putDown item em = let
  ix = 1 + maximum (Map.keys em)
  in Map.insert ix item em
