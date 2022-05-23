{-# LANGUAGE OverloadedStrings #-}
{-

Game.Birth.hs

Game.Birth.hs creates the Player (@)

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Birth (mkPlayer) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Kind.Entity (EntityKind(..))
import Game.Rules

type Player = EntityKind
type Seed = Int

-- | archtype
archtype :: Int -> Int -> Int -> Int -> Text
archtype str dex int wis = let
  warrior  = str + dex
  academic = int + wis
  career
    | warrior > academic  && str > dex = "Fighter"
    | warrior > academic  && dex > str = "Rogue"
    | academic > warrior  && int > wis = "Mage"
    | academic > warrior  && wis > int = "Cleric"
    | warrior == academic && str > int || str > wis = "Fighter"
    | warrior == academic && dex > int || dex > wis = "Rogue"
    | warrior == academic && int > str || int > dex = "Mage"
    | warrior == academic && wis > str || wis > dex = "Cleric"
    | otherwise = "Fighter"
  in career

-- | healthPool
healthPool :: Text -> (Text, Text)
healthPool n
  | n == "Fighter" = ("HP", "10")
  | n == "Rogue"   = ("HP", "8")
  | n == "Mage"    = ("HP", "8")
  | n == "Cleric"  = ("HP", "8")
  | otherwise = ("HP", "8")

-- | manaPool
manaPool :: Text -> (Text, Text)
manaPool n
  | n == "Fighter" = ("MP", "0")
  | n == "Rogue"   = ("MP", "6")
  | n == "Mage"    = ("MP", "10")
  | n == "Cleric"  = ("MP", "8")
  | otherwise = ("MP", "0")

-- | @ Stats
mkPlayer :: Seed -> Player -> Player
mkPlayer s pEntity = let
  pProp = property pEntity
  aAC  = read $ T.unpack $ Map.findWithDefault "10" "AC" pProp
  pAC  = abilityMod rDex + aAC
  -- random stats
  rStr = DS.d3 (s+1)  + DS.d4 (s+2)  + DS.d5 (s+3)  + DS.d6 (s+4)
  rDex = DS.d3 (s+5)  + DS.d4 (s+6)  + DS.d5 (s+7)  + DS.d6 (s+8)
  rCon = DS.d3 (s+9)  + DS.d4 (s+10) + DS.d5 (s+11) + DS.d6 (s+12)
  rInt = DS.d3 (s+13) + DS.d4 (s+14) + DS.d5 (s+15) + DS.d6 (s+16)
  rWis = DS.d3 (s+17) + DS.d4 (s+18) + DS.d5 (s+19) + DS.d6 (s+20)
  pClass = archtype rStr rDex rInt rWis
  newProp = Map.fromList [ ("str", T.pack $ show rStr)
            , ("dex", T.pack $ show rDex)
            , ("con", T.pack $ show rCon)
            , ("int", T.pack $ show rInt)
            , ("wis", T.pack $ show rWis)
            , ("AC",  T.pack $ show pAC)
            , ("Class", pClass)
            , healthPool pClass
            , manaPool pClass
            , weaponAttack pClass
            , ("Attacks", "1")
            , weaponCast pClass
            , weaponClass pClass
            , weaponWT pClass
            ]
  in pEntity { property = Map.union newProp pProp }

-- | @ Weapons
weaponAttack :: Text -> (Text,Text)
weaponAttack n
  | n == "Fighter" = ("ATTACK", "1d8")
  | n == "Rogue"   = ("ATTACK", "1d4")
  | n == "Mage"    = ("ATTACK", "1d6")
  | n == "Cleric"  = ("ATTACK", "1d6")
  | otherwise      = ("ATTACK", "1d4")

-- | @ Weapons
-- Extra Cast Damage based on level
weaponCast :: Text -> (Text,Text)
weaponCast n
  | n == "Fighter" = ("CAST", "0")
  | n == "Rogue"   = ("CAST", "1d6")
  | n == "Mage"    = ("CAST", "1d10")
  | n == "Cleric"  = ("CAST", "1d8")
  | otherwise      = ("CAST", "0")

-- | @ Weapons
weaponClass :: Text -> (Text,Text)
weaponClass n
  | n == "Fighter" = ("melee", "melee/Longsword")
  | n == "Rogue"   = ("shoot", "shoot/Throwing Knife")
  | n == "Mage"    = ("melee", "melee/Quarterstaff")
  | n == "Cleric"  = ("melee", "melee/Mace")
  | otherwise      = ("melee", "melee/Dagger")

-- | @ Weapons
weaponWT :: Text -> (Text,Text)
weaponWT n
  | n == "Fighter" = ("WWT", "3")
  | n == "Rogue"   = ("WWT", "1")
  | n == "Mage"    = ("WWT", "4")
  | n == "Cleric"  = ("WWT", "4")
  | otherwise      = ("WWT", "1")
