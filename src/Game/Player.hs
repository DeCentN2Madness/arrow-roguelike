{-# LANGUAGE OverloadedStrings #-}
{-

Game.Player.hs

Game.Player is the engine for the Actor EntityKind. This is used
as interchangable helper functions for id=0

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
  in [ pLvl, pExp, " ", pStr, pDex, pCon, pInt, pWis, " ", pHP ]

-- | @ Inv
characterInventory :: EntityMap -> [Text]
characterInventory em = let
  (pEntity, _) = getPlayer em
  pInv   = inventory pEntity
  pCoin  = T.pack $ "Coin: "     ++ show (Map.findWithDefault 0 "Coin"     pInv)
  pMush  = T.pack $ "Mushroom: " ++ show (Map.findWithDefault 0 "Mushroom" pInv)
  pPot   = T.pack $ "Potion: "   ++ show (Map.findWithDefault 0 "Potion"   pInv)
  pUnk   = T.pack $ "Unknown: "  ++ show (Map.findWithDefault 0 "Unknown"  pInv)
  in [ pCoin, pMush, pPot, pUnk ]

-- | @ lives at 0
-- get Player
getPlayer :: EntityMap -> (Player, Coord)
getPlayer = GE.getEntityAt 0

proficiency :: Int -> Int
proficiency lvl
  | lvl >= 1  && lvl <= 4 = 2
  | lvl >= 5  && lvl <= 8 = 3
  | lvl >= 9  && lvl <= 12 = 4
  | lvl >= 13 && lvl <= 16 = 5
  | lvl >= 17 && lvl <= 20 = 6
  | otherwise = 2

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
  newProp = Map.insert "Proficiency" (show $ proficiency pLvl) pProp
  newPlayer = pEntity { property=newProp, eLvl=pLvl, eHP=pHP, eMaxHP=pMaxHP, eXP=pTot }
  in updatePlayer newPlayer em

-- | xpLevel simple
xpLevel :: Int -> Int
xpLevel x
  | x > 0    && x <= 35  = 1
  | x > 35   && x <= 100 = 2
  | x > 100  && x <= 200 = 3
  | x > 200  && x <= 300 = 4
  | x > 300  && x <= 400 = 5
  | x > 400  && x <= 600 = 6
  | x > 600  && x <= 800 = 7
  | x > 800  && x <= 1000 = 8
  | x > 1000 && x <= 1200 = 9
  | x > 1200 && x <= 1400 = 10
  | x > 1400 && x <= 1600 = 11
  | x > 1600 && x <= 2000 = 12
  | x > 2000 && x <= 3000 = 13
  | x > 3000 && x <= 4600 = 14
  | x > 4000 && x <= 5000 = 15
  | x > 5000 && x <= 6000 = 16
  | x > 6000 && x <= 7000 = 17
  | x > 7000 && x <= 8000 = 18
  | x > 8000 && x <= 10000 = 19
  | x > 10000              = 20
  | otherwise = 1
