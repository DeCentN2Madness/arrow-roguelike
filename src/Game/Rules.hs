{-# LANGUAGE OverloadedStrings #-}
{-

Game.Rules.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Rules where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS

-- | abilityGain
-- @ stats gain w/ lvl
abilityGain :: Int -> Int -> (Int, Int)
abilityGain lvl curr
  | lvl == 4  && lvl > curr = (2, 0)
  | lvl == 8  && lvl > curr = (2, 0)
  | lvl == 12 && lvl > curr = (0, 2)
  | lvl == 16 && lvl > curr = (1, 1)
  | lvl == 19 && lvl > curr = (1, 1)
  | otherwise = (0,0)

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | abilityResult
abilityResult :: Int -> Int -> Int -> Int -> Text
abilityResult result roll modifier prof =
  T.concat [ abilityR1 result
           , " ["
           , T.pack $ show roll
           , ", "
           , resultFmt modifier
           , ", "
           , resultFmt prof
           , "]"
           ]

-- | abilityResult
abilityR1 :: Int -> Text
abilityR1 result =
  T.concat [ " <"
           , T.pack $ show result
           , ">"
           ]

-- | abilityResult
abilityR2 :: Int -> Int -> Text
abilityR2 resultA resultB =
  T.concat [ " <"
           , T.pack $ show resultA
           , ", "
           , T.pack $ show resultB
           , ">"
           ]

-- | abilityResult2
abilityResult2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Text
abilityResult2 resultA resultB rollA rollB modA modB prof =
  T.concat [ abilityR2 resultA resultB
           , " ["
           , T.pack $ show rollA
           , ", "
           , T.pack $ show rollB
           , ", "
           , resultFmt modA
           , ", "
           , resultFmt modB
           , ", "
           , resultFmt prof
           , "]"
           ]

-- | attacksGain
-- @ gains ATTACKS w/ lvl
attacksGain :: Text -> Int -> Int -> Text -> Text
attacksGain pCls lvl curr attacks
  | pCls == "Fighter" && lvl == 5  && lvl > curr = "2d8"
  | pCls == "Fighter" && lvl == 11 && lvl > curr = "3d8"
  | pCls == "Fighter" && lvl == 17 && lvl > curr = "4d8"
  | pCls == "Fighter" && lvl == 18 && lvl > curr = "4d8+4"
  | pCls == "Fighter" && lvl == 19 && lvl > curr = "4d8+6"
  | pCls == "Fighter" && lvl == 20 && lvl > curr = "4d8+8"
  | pCls == "Rogue"   && lvl == 5  && lvl > curr = "2d6"
  | pCls == "Rogue"   && lvl == 11 && lvl > curr = "3d6"
  | pCls == "Rogue"   && lvl == 17 && lvl > curr = "4d6"
  | pCls == "Rogue"   && lvl == 18 && lvl > curr = "4d6+2"
  | pCls == "Rogue"   && lvl == 19 && lvl > curr = "4d6+4"
  | pCls == "Rogue"   && lvl == 20 && lvl > curr = "4d6+6"
  | otherwise = attacks

-- | castGain
-- @ gains CAST w/ lvl
castGain :: Text -> Int -> Int -> Text -> Text
castGain pCls lvl curr cast
  | pCls == "Rogue"  && lvl == 5  && lvl > curr = "2d4"
  | pCls == "Rogue"  && lvl == 11 && lvl > curr = "3d4"
  | pCls == "Rogue"  && lvl == 17 && lvl > curr = "4d4"
  | pCls == "Rogue"  && lvl == 18 && lvl > curr = "4d4+2"
  | pCls == "Rogue"  && lvl == 19 && lvl > curr = "4d4+3"
  | pCls == "Rogue"  && lvl == 20 && lvl > curr = "4d4+4"
  | pCls == "Mage"   && lvl == 5  && lvl > curr = "2d10"
  | pCls == "Mage"   && lvl == 11 && lvl > curr = "3d10"
  | pCls == "Mage"   && lvl == 17 && lvl > curr = "4d10"
  | pCls == "Mage"   && lvl == 18 && lvl > curr = "4d10+6"
  | pCls == "Mage"   && lvl == 19 && lvl > curr = "4d10+8"
  | pCls == "Mage"   && lvl == 20 && lvl > curr = "4d10+10"
  | pCls == "Cleric" && lvl == 5  && lvl > curr = "2d8"
  | pCls == "Cleric" && lvl == 11 && lvl > curr = "3d8"
  | pCls == "Cleric" && lvl == 17 && lvl > curr = "4d8"
  | pCls == "Cleric" && lvl == 18 && lvl > curr = "4d8+4"
  | pCls == "Cleric" && lvl == 19 && lvl > curr = "4d8+6"
  | pCls == "Cleric" && lvl == 20 && lvl > curr = "4d8+8"
  | otherwise = cast

-- | checkEncumberance
-- @ loses proficiency based on WT
checkEncumberance :: Int -> Int -> Int -> Int
checkEncumberance str wt prof
  | wt > 10 * str = -5
  | wt > 9 * str  = -4
  | wt > 8 * str  = -3
  | wt > 7 * str  = -2
  | wt > 6 * str  = -1
  | wt > 5 * str  = 0
  | otherwise = prof

-- | checkFinesse
-- @ Rogue loses Attacks if Heavy WWT
checkFinesse :: Int -> Text -> Text
checkFinesse wwt prof
  | wwt < 3   = prof
  | otherwise = "0"

-- | checkFinessMagic
-- @ Magic User loses Cast if Heavy WWT
checkFinesseMagic :: Int -> Text -> Text
checkFinesseMagic wwt prof
  | wwt < 5   = prof
  | otherwise = "0"

-- | criticalDamage
-- @ can Crit!
criticalDamage :: Int -> Text -> Int -> Int -> Int
criticalDamage roll pWeap s modifier
  | roll == 1   = 0
  | roll == 100 = result + crit
  | otherwise   = if result < 1 then 0 else result
  where
    result = weapon pWeap (s+1) modifier
    crit   = weapon pWeap (s+2) modifier

-- | criticalRoll
-- One or Twenty
--   a. 1 is always miss
--   b. 20 is always hit
criticalRoll :: Int -> Int -> Int -> Int
criticalRoll roll modifier prof
  | roll == 1  = 1
  | roll == 20 = 100
  | otherwise  = if result < 1 then 1 else result
  where
    result = roll + modifier + prof

-- | @ gets Proficient w/ lvl
proficiency :: Int -> Int
proficiency lvl
  | lvl >= 1  && lvl <= 4  = 2
  | lvl >= 5  && lvl <= 8  = 3
  | lvl >= 9  && lvl <= 12 = 4
  | lvl >= 13 && lvl <= 16 = 5
  | lvl >= 17 && lvl <= 20 = 6
  | otherwise = 2

-- | resultFmt
resultFmt :: Int -> Text
resultFmt n
  | n < 0 = T.pack $ show n
  | n > 1 = T.append "+" $ T.pack $ show n
  | otherwise = "+0"

-- | weapon
-- Damage roll for Weapons...
-- Example: 1d4+1
weapon :: Text -> Int -> Int -> Int
weapon dice seed bonus = let
  (wDam, wMod) = T.breakOn "+" dice
  roll = case wDam of
    "0"   -> 0
    "1d1" -> 1
    "1d2" -> DS.d2 seed
    "1d3" -> DS.d3 seed
    "1d4" -> DS.d4 seed
    "1d5" -> DS.d5 seed
    "1d6" -> DS.d6 seed
    "1d8" -> DS.d8 seed
    "1d10" -> DS.d10 seed
    "1d12" -> DS.d12 seed
    -- d2, d3, d5
    "2d2" -> DS.d2 seed + DS.d2 (seed+1)
    "2d3" -> DS.d3 seed + DS.d3 (seed+1)
    "2d5" -> DS.d5 seed + DS.d5 (seed+1)
    "3d2" -> DS.d2 seed + DS.d2 (seed+1) + DS.d2 (seed+2)
    "3d3" -> DS.d3 seed + DS.d3 (seed+1) + DS.d3 (seed+2)
    "3d5" -> DS.d5 seed + DS.d5 (seed+1) + DS.d5 (seed+2)
    -- d4
    "2d4" -> DS.d4 seed + DS.d4 (seed+1)
    "3d4" -> DS.d4 seed + DS.d4 (seed+1) + DS.d4 (seed+2)
    "4d4" -> DS.d4 seed + DS.d4 (seed+1) + DS.d4 (seed+2) + DS.d4 (seed+3)
    -- d6
    "2d6" -> DS.d6 seed + DS.d6 (seed+1)
    "3d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2)
    "4d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2) + DS.d6 (seed+3)
    -- d8
    "2d8" -> DS.d8 seed + DS.d8 (seed+1)
    "3d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2)
    "4d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2) + DS.d8 (seed+3)
    -- d10
    "2d10" -> DS.d10 seed + DS.d10 (seed+1)
    "3d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2)
    "4d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2) + DS.d10 (seed+3)
    -- d12
    "2d12" -> DS.d12 seed + DS.d12 (seed+1)
    "3d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2)
    "4d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2) + DS.d12 (seed+3)
    _ -> DS.d4 seed
  wBonus n
    | n == "-10" = -10
    | n == "-9" = -9
    | n == "-8" = -8
    | n == "-7" = -7
    | n == "-6" = -6
    | n == "-5" = -5
    | n == "-4" = -4
    | n == "-3" = -3
    | n == "-2" = -2
    | n == "-1" = -1
    | n == "+0" = 0
    | n == "+1" = 1
    | n == "+2" = 2
    | n == "+3" = 3
    | n == "+4" = 4
    | n == "+5" = 5
    | n == "+6" = 6
    | n == "+7" = 7
    | n == "+8" = 8
    | n == "+9" = 9
    | n == "+10" = 10
    | otherwise = 0
  in roll + bonus + wBonus wMod

-- | xpLevel simple
xpLevel :: Int -> Int
xpLevel x
  | x > 0     && x <= 35  = 1
  | x > 35    && x <= 100 = 2
  | x > 100   && x <= 200 = 3
  | x > 200   && x <= 300 = 4
  | x > 300   && x <= 800 = 5
  | x > 800   && x <= 1300 = 6
  | x > 1300  && x <= 1800 = 7
  | x > 1800  && x <= 2300 = 8
  | x > 2300  && x <= 3300 = 9
  | x > 3300  && x <= 4300 = 10
  | x > 4300  && x <= 5300 = 11
  | x > 5300  && x <= 6300 = 12
  | x > 6300  && x <= 7300 = 13
  | x > 7300  && x <= 8300 = 14
  | x > 8300  && x <= 10000 = 15
  | x > 10000 && x <= 12000 = 16
  | x > 12000 && x <= 14000 = 17
  | x > 14000 && x <= 16000 = 18
  | x > 16000 && x <= 18000 = 19
  | x > 18000 = 20
  | otherwise = 1
