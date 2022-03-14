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
import Game.Entity (EntityMap)
import Game.Kind.Entity

type Coord = (Int, Int)
type AssetMap = EntityMap
type NameMap = Map String EntityKind

-- | emptyBy
-- Get the list of picked items...
emptyBy :: Coord -> [(EntityKind, Coord)] -> EntityMap -> EntityMap
emptyBy pos items em = let
  entityIX = [ (xy, ix, e) | (ix, e) <- Map.toList em, let xy = coord e ]
  deleteList = [ d | (xy, i, e) <- entityIX,
                 let d = if xy == pos && kind e `elem` pickList items
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
                let eProp = property ek
                    name  = Map.findWithDefault "0" "Name" eProp ]
  in Map.fromList assetList

-- | mkItem
mkItem :: String -> Coord -> AssetMap -> EntityKind
mkItem name pos am = let
  assets = mkNameMap am
  arr    = mkEntity Arrow pos
  item   = Map.findWithDefault arr name assets
  in item { coord = pos }

-- | noPickup
noPickup :: [Entity]
noPickup = [Actor, Corpse, Monster, StairDown, StairUp, Trap]

-- | pickList
pickList :: [(EntityKind, (Int, Int))] -> [Entity]
pickList items = filter (`notElem` noPickup) $ [ kind e | (e, _) <- items ]

-- | pickUp
pickUp :: [(EntityKind, Coord)] -> EntityKind -> EntityKind
pickUp items ek = let
  invT  = groupEK $ map show (pickList items)
  picks = Map.unionWith (+) (inventory ek) (Map.fromList invT)
  in ek { inventory = picks }

-- | putDown
putDown :: EntityKind -> EntityMap -> EntityMap
putDown item em = let
  ix = 1 + length (Map.keys em)
  in Map.insert ix item em
