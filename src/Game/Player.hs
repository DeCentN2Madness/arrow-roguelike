{-# LANGUAGE OverloadedStrings #-}
{-

Game.Player.hs

Game.Player is the engine for the Actor EntityKind. This is used
as interchangable helper functions for just id=0

Example: getPlayer returns the Player from the EntityMap

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Player (characterSheet
                   , characterInventory
                   , getPlayer
                   , updatePlayerBy
                   , updatePlayer
                   , updatePlayerXP) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))

type Coord = (Int, Int)
type Player = EntityKind

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | @ Stats
characterSheet :: EntityMap -> [Text]
characterSheet em = let
  (pEntity, _) = getPlayer em
  pProp = property pEntity
  pStr  = T.pack $ "STR: " ++ Map.findWithDefault "1" "str" pProp
  pDex  = T.pack $ "DEX: " ++ Map.findWithDefault "1" "dex" pProp
  pCon  = T.pack $ "CON: " ++ Map.findWithDefault "1" "con" pProp
  pInt  = T.pack $ "INT: " ++ Map.findWithDefault "1" "int" pProp
  pWis  = T.pack $ "WIS: " ++ Map.findWithDefault "1" "wis" pProp
  pLvl  = T.pack $ "Level: " ++ show (eLvl pEntity)
  pExp  = T.pack $ "EXP: " ++ show (eXP pEntity)
  pHP   = T.pack $ "HP: " ++ show (eHP pEntity) ++ "/" ++ show (eMaxHP pEntity)
  in [ "@", pLvl, pExp, " ", pStr, pDex, pCon, pInt, pWis, " ", pHP ]

-- | @ Inv
characterInventory :: EntityMap -> [Text]
characterInventory em = let
  (pEntity, _) = getPlayer em
  pInv   = inventory pEntity
  pCoin  = T.pack $ "Coin: "     ++ show (Map.findWithDefault 0 "Coin"     pInv)
  pMush  = T.pack $ "Mushroom: " ++ show (Map.findWithDefault 0 "Mushroom" pInv)
  pPot   = T.pack $ "Potion: "   ++ show (Map.findWithDefault 0 "Potion"   pInv)
  pUnk   = T.pack $ "Unknown: "  ++ show (Map.findWithDefault 0 "Unknown"  pInv)
  in [ "@", pCoin, pMush, pPot, pUnk ]

-- | @ lives at 0
-- get Player
getPlayer :: EntityMap -> (Player, Coord)
getPlayer = GE.getEntityAt 0

-- | update @ properties
updatePlayer :: Player -> EntityMap -> EntityMap
updatePlayer = Map.insert 0

-- | update @ position
updatePlayerBy :: Coord -> EntityMap -> EntityMap
updatePlayerBy = GE.updateEntityPos 0

-- | updateEntityXP at ix
updatePlayerXP :: Int -> EntityMap -> EntityMap
updatePlayerXP xp em = let
  (pEntity, _ ) = getPlayer em
  pProp = property pEntity
  pCon  = read $ Map.findWithDefault "1" "con" pProp :: Int
  pTot = eXP pEntity + xp
  pLvl = xpLevel pTot
  pHP = if pLvl > eLvl pEntity then pMaxHP else eHP pEntity
  pMaxHP = pLvl * (10 + abilityMod pCon)
  in updatePlayer (pEntity { eLvl=pLvl, eHP=pHP, eMaxHP=pMaxHP, eXP=pTot }) em

-- | xpLevel simple
xpLevel :: Int -> Int
xpLevel n
  | n > 0   && n <= 30  = 1
  | n > 30  && n <= 100 = 2
  | n > 100 && n <= 200 = 3
  | n > 200 && n <= 300 = 4
  | n > 300 && n <= 400 = 5
  | n > 400 && n <= 500 = 6
  | n > 500 && n <= 600 = 7
  | n > 600 && n <= 700 = 8
  | n > 700 && n <= 800 = 9
  | n > 800             = 10
  | otherwise = 1
