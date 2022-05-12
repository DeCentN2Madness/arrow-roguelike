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
-- @ stat gain at levels 4, 8, 12, 16, 19
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

-- | criticalDamage
-- @ can Crit!
criticalDamage :: Int -> Text -> Int -> Int -> Int
criticalDamage roll pWeap s modifier
  | roll == 1   = 0
  | roll == 100 = weapon pWeap s modifier + weapon pWeap (s+1) 0
  | otherwise   = weapon pWeap s modifier

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

-- | @ gets better with level
proficiency :: Int -> Int
proficiency lvl
  | lvl >= 1  && lvl <= 4 = 2
  | lvl >= 5  && lvl <= 8 = 3
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
    "1d1" -> 1
    "1d2" -> DS.d2 seed
    "1d3" -> DS.d3 seed
    "1d4" -> DS.d4 seed
    "1d5" -> DS.d5 seed
    "1d6" -> DS.d6 seed
    "1d8" -> DS.d8 seed
    "1d10" -> DS.d10 seed
    "1d12" -> DS.d12 seed
    "2d2" -> DS.d2 seed + DS.d2 (seed+1)
    "2d3" -> DS.d3 seed + DS.d3 (seed+1)
    "2d4" -> DS.d4 seed + DS.d4 (seed+1)
    "2d5" -> DS.d5 seed + DS.d5 (seed+1)
    "2d6" -> DS.d6 seed + DS.d6 (seed+1)
    _     -> DS.d4 seed
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
