{-

Game.Inventory.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Inventory (pickup) where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map as Map
import Game.Kind.Entity (Entity(..), EntityKind(..))

type Coord = (Int, Int)

groupEK :: [Entity] -> [(Entity, Int)]
groupEK = map (head &&& length) . group . sort

noPickup :: [Entity]
noPickup = [Actor, Mouse, Corpse, StairDown, StairUp, Trap]

-- | pickup
pickup :: [(EntityKind, Coord)] -> EntityKind -> EntityKind
pickup xs ek = let
  pProp = Map.toList $ prop ek
  entityList = filter (`notElem` noPickup) $ [ kind e | (e, _) <- xs ]
  em = [ ("pickup", show $ groupEK entityList) ]
  picks = Map.fromList $ concat [pProp, em]
  in ek { prop = picks }
