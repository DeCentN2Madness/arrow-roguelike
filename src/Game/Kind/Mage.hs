{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Mage.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Mage where

import Data.Text (Text)

-- | abilityGain
-- @ Primary+6, Secondary+4 stats gain w/ lvl
abilityGain :: Int -> Int -> (Int, Int)
abilityGain lvl curr
  | lvl == 4  && lvl > curr = (2, 0)
  | lvl == 8  && lvl > curr = (2, 0)
  | lvl == 12 && lvl > curr = (0, 2)
  | lvl == 16 && lvl > curr = (1, 1)
  | lvl == 19 && lvl > curr = (1, 1)
  | otherwise = (0,0)

-- | attacksGain
-- @ Mage gain ATTACKS w/ lvl
attacksGain :: Int -> Text -> Text
attacksGain _ _ = "1"

-- | castGain
-- @ Mage gain CAST w/ lvl
castGain :: Int -> Text -> Text
castGain lvl cast
  | lvl == 1  = "1d10"
  | lvl == 5  = "2d10"
  | lvl == 11 = "3d10+10"
  | lvl == 17 = "4d10+10"
  | lvl == 18 = "5d10+10"
  | lvl == 19 = "6d10+10"
  | lvl == 20 = "7d10+10"
  | otherwise = cast

-- | @ gets Proficient w/ lvl
proficiencyGain :: Int -> Text -> Text
proficiencyGain lvl prof
  | lvl == 1  = "2"
  | lvl == 5  = "3"
  | lvl == 9  = "4"
  | lvl == 13 = "5"
  | lvl == 17 = "6"
  | otherwise = prof

-- | @ gets Search w/ lvl
searchGain :: Int -> Text -> Text
searchGain lvl search
  | lvl == 5  = "5"
  | lvl == 10 = "6"
  | lvl == 15 = "7"
  | otherwise = search
