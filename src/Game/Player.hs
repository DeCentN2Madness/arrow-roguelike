{-

Game.Player.hs

Game.Player is the engine for the Actor EntityKind. This is used
as interchangable helper functions for just id=0

Example: getPlayer returns the Player from the EntityMap

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Player(getPlayer
               , updatePlayerBy
               , updatePlayer
               , updatePlayerXP
               ) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))

type Player = EntityKind

-- | @ lives at 0
-- get Player
getPlayer :: EntityMap -> (Player, (Int,Int))
getPlayer = GE.getEntityAt 0

-- | update @ properties
updatePlayer :: Player -> EntityMap -> EntityMap
updatePlayer = Map.insert 0

-- | update @ position
updatePlayerBy :: (Int, Int) -> EntityMap -> EntityMap
updatePlayerBy = GE.updateEntityPos 0

-- | updateEntityXP at ix
updatePlayerXP :: Int -> EntityMap -> EntityMap
updatePlayerXP xp em = let
  (Just ek) = Map.lookup 0 em
  pTot = eXP ek + xp
  pLvl = xpLevel pTot
  pHP = if pLvl > eLvl ek then pMaxHP else eHP ek
  pMaxHP = pLvl * 10
  in Map.insert 0 (ek { eLvl=pLvl, eHP=pHP, eMaxHP=pMaxHP, eXP=pTot }) em

-- | xpLevel simple
xpLevel :: Int -> Int
xpLevel n
  | n > 30  && n < 100 = 2
  | n > 100 && n < 200 = 3
  | n > 200 && n < 300 = 4
  | n > 300 = 5
  | otherwise = n
