{-# LANGUAGE OverloadedStrings #-}
{-

Game.Birth.hs

Game.Birth.hs creates the Player (@)

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Birth (mkPlayer) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Kind.Entity (EntityKind(..))

type Player = EntityKind
type Seed = Int

abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | @ Stats
-- TODO AC, HP, MP based on class
mkPlayer :: Seed -> Player -> Player
mkPlayer s pEntity = let
  pProp  = property pEntity
  aAC  = read $ T.unpack $ Map.findWithDefault "10" "AC" pProp
  pAC  = abilityMod rDex + aAC
  -- random stats
  rStr = DS.d3 (s+1)  + DS.d4 (s+2)  + DS.d5 (s+3)  + DS.d6 (s+4)
  rDex = DS.d3 (s+5)  + DS.d4 (s+6)  + DS.d5 (s+7)  + DS.d6 (s+8)
  rCon = DS.d3 (s+9)  + DS.d4 (s+10) + DS.d5 (s+11) + DS.d6 (s+12)
  rInt = DS.d3 (s+13) + DS.d4 (s+14) + DS.d5 (s+15) + DS.d6 (s+16)
  rWis = DS.d3 (s+17) + DS.d4 (s+18) + DS.d5 (s+19) + DS.d6 (s+20)
  pStr = Map.insert "str" (T.pack $ show rStr) pProp
  pDex = Map.insert "dex" (T.pack $ show rDex) pStr
  pCon = Map.insert "con" (T.pack $ show rCon) pDex
  pInt = Map.insert "int" (T.pack $ show rInt) pCon
  pWis = Map.insert "wis" (T.pack $ show rWis) pInt
  pArmor = Map.insert "AC" (T.pack $ show pAC) pWis
  in pEntity { property = pArmor }
