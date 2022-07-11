{-# LANGUAGE OverloadedStrings #-}
{-

Game.Rules.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Rules where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Kind.Entity

-- | abilityBonus
-- Formatted bonus
abilityBonus :: Text -> Text
abilityBonus n = resultFmt $ abilityMod $ read $ T.unpack n

-- | abilityLookup
-- @ ability with modifiers like Items, Status, etc...
abilityLookup :: Text -> EntityKind -> (Int, Int)
abilityLookup stat pEntity = let
  pProp = property pEntity
  pStat = read $ T.unpack $ Map.findWithDefault "0" stat pProp :: Int
  pTag  = T.append "+" (T.toUpper stat)
  pTags = T.splitOn "," $ Map.findWithDefault "," "TAGS" pProp
  pCnt  = length $ filter (==pTag) pTags
  newStat = pStat + pCnt
  pMod  = abilityMod newStat
  in (newStat, pMod)

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

-- | checkEncumberance
-- @ loses Proficiency based on WT
checkEncumberance :: Int -> Int -> Int -> Int
checkEncumberance str wt prof
  | wt > (8 * str) = -5
  | wt > (7 * str) = -3
  | wt > (6 * str) = -1
  | wt > (5 * str) = 0
  | otherwise = prof

-- | checkFinesse
-- @ Magic User loses CAST, Proficiency if Heavy WWT
checkFinesse :: Int -> Text -> Text
checkFinesse wwt prof
  | wwt < 5   = prof
  | otherwise = "0"

-- | criticalDamage
-- @ can Crit!
criticalDamage :: Int -> Text -> Int -> Int -> Int
criticalDamage roll pWeap pSeed pStat
  | roll == 1   = 0
  | roll == 100 = result + crit
  | otherwise   = if result < 1 then 0 else result
  where
    result = weapon pWeap (pSeed+1) pStat
    crit   = weapon pWeap (pSeed+2) pStat

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

-- | itemLookup
-- @ Items...
itemLookup :: Text -> EntityKind -> AssetMap -> Text
itemLookup x pEntity am = let
  mkDescMap :: Map Text Text
  mkDescMap = let
    assetList = [ (n, d) | (_, ek) <- Map.toList am,
                  let n = Map.findWithDefault "None" "Name" (property ek)
                      d = Map.findWithDefault "None" "Description" (property ek) ]
    in Map.fromList assetList
  descMap = mkDescMap
  item = propertyLookup x pEntity
  desc = Map.findWithDefault "" item descMap
  label = if item /= "None" then item else T.concat [ x, "/", "None" ]
  in T.concat [ T.justifyLeft 30 ' ' label, " ", desc ]

-- | propertyLookup
-- @ properties as Text...
propertyLookup :: Text -> EntityKind -> Text
propertyLookup stat pEntity = let
  pProp = property pEntity
  pStat = Map.findWithDefault "None" stat pProp
  in pStat

-- | propertyIntLookup
-- @ properties as Int...
propertyNLookup :: Text -> EntityKind -> Int
propertyNLookup stat pEntity = let
  pProp = property pEntity
  pStat = read $ T.unpack $ Map.findWithDefault "0" stat pProp :: Int
  in pStat

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
weapon pWeap seed bonus = let
  (wDam, wMod) = T.breakOn "+" pWeap
  roll = case wDam of
    "None" -> 1
    "0"    -> 0
    "1"    -> 1
    "1d1"  -> 1
    -- d4, d6, d8, d10, d12, d20
    "1d4" -> DS.d4 seed
    "1d6" -> DS.d6 seed
    "1d8" -> DS.d8 seed
    "1d10" -> DS.d10 seed
    "1d12" -> DS.d12 seed
    "1d20" -> DS.d20 seed
    -- d2, d3, d5
    "1d2" -> DS.d2 seed
    "2d2" -> DS.d2 seed + DS.d2 (seed+1)
    "3d2" -> DS.d2 seed + DS.d2 (seed+1) + DS.d2 (seed+2)
    "1d3" -> DS.d3 seed
    "2d3" -> DS.d3 seed + DS.d3 (seed+1)
    "3d3" -> DS.d3 seed + DS.d3 (seed+1) + DS.d3 (seed+2)
    "1d5" -> DS.d5 seed
    "2d5" -> DS.d5 seed + DS.d5 (seed+1)
    "3d5" -> DS.d5 seed + DS.d5 (seed+1) + DS.d5 (seed+2)
    -- d4
    "2d4" -> DS.d4 seed + DS.d4 (seed+1)
    "3d4" -> DS.d4 seed + DS.d4 (seed+1) + DS.d4 (seed+2)
    "4d4" -> DS.d4 seed + DS.d4 (seed+1) + DS.d4 (seed+2) + DS.d4 (seed+3)
    -- d6
    "2d6" -> DS.d6 seed + DS.d6 (seed+1)
    "3d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2)
    "4d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2) + DS.d6 (seed+3)
    "5d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2) + DS.d6 (seed+3) + DS.d6 (seed+4)
    "6d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2) + DS.d6 (seed+3) + DS.d6 (seed+4) + DS.d6 (seed+5)
    "7d6" -> DS.d6 seed + DS.d6 (seed+1) + DS.d6 (seed+2) + DS.d6 (seed+3) + DS.d6 (seed+4)+ DS.d6 (seed+5) + DS.d6 (seed+6)
    -- d8
    "2d8" -> DS.d8 seed + DS.d8 (seed+1)
    "3d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2)
    "4d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2) + DS.d8 (seed+3)
    "5d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2) + DS.d8 (seed+3) + DS.d8 (seed+4)
    "6d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2) + DS.d8 (seed+3) + DS.d8 (seed+4) + DS.d8 (seed+5)
    "7d8" -> DS.d8 seed + DS.d8 (seed+1) + DS.d8 (seed+2) + DS.d8 (seed+3) + DS.d8 (seed+4) + DS.d8 (seed+5) + DS.d8 (seed+6)
    -- d10
    "2d10" -> DS.d10 seed + DS.d10 (seed+1)
    "3d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2)
    "4d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2) + DS.d10 (seed+3)
    "5d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2) + DS.d10 (seed+3) + DS.d10 (seed+4)
    "6d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2) + DS.d10 (seed+3) + DS.d10 (seed+4) + DS.d10 (seed+5)
    "7d10" -> DS.d10 seed + DS.d10 (seed+1) + DS.d10 (seed+2) + DS.d10 (seed+3) + DS.d10 (seed+4) + DS.d10 (seed+5) + DS.d10 (seed+6)
    -- d12
    "2d12" -> DS.d12 seed + DS.d12 (seed+1)
    "3d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2)
    "4d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2) + DS.d12 (seed+3)
    "5d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2) + DS.d12 (seed+3) + DS.d12 (seed+4)
    "6d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2) + DS.d12 (seed+3) + DS.d12 (seed+4) + DS.d12 (seed+5)
    "7d12" -> DS.d12 seed + DS.d12 (seed+1) + DS.d12 (seed+2) + DS.d12 (seed+3) + DS.d12 (seed+4) + DS.d12 (seed+5) + DS.d12 (seed+6)
    _ -> DS.d4 seed
  wBonus n
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
