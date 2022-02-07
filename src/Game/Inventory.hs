{-

Game.Inventory.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Inventory (emptyBy
                       , pickup) where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map.Strict as Map
import Game.Actor (EntityMap)
import Game.Kind.Entity (Entity(..), EntityKind(..))

-- | emptyBy
-- Get the list of picked items
-- Modify the map
emptyBy :: (Int, Int) -> [(EntityKind, (Int, Int))] -> EntityMap -> EntityMap
emptyBy pos items em = let
    pickList = filter (`notElem` noPickup) $ [ kind e | (e, _) <- items ]
    entityIX = [ (xy, ix, e) | (ix, e) <- Map.toList em,
                 let xy = coord e ]
    deleteList = [ d | (xy, i, e) <- entityIX,
                   let d = if xy == pos &&  kind e `elem` pickList
                         then i else (-1) ]
    newEntity = filter ((/=(-1)).fst) $ [ (ix, ek) | (i, ek) <- Map.toList em,
                  let ix = if i `notElem` deleteList then i else (-1) ]
  in if not (null deleteList) then Map.fromList newEntity else em

-- | groupEK builds counts from a list
groupEK :: [String] -> [(String, Int)]
groupEK = map (head &&& length) . group . sort

-- | noPickup
noPickup :: [Entity]
noPickup = [Actor, Mouse, Corpse, StairDown, StairUp, Trap, Unknown]

-- | pickup
pickup :: [(EntityKind, (Int, Int))] -> EntityKind -> EntityKind
pickup items ek = let
  invT = let
    pickList = filter (`notElem` noPickup) $ [ kind e | (e, _) <- items ]
    in groupEK $ map show pickList
  picks = Map.unionWith (+) (inventory ek) (Map.fromList invT)
  in ek { inventory = picks }
