{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Rogue.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Rogue where

import Data.Text (Text)

-- | abilityGain
-- @ Primary+8, Secondary+4 stats gain w/ lvl
abilityGain :: Int -> Int -> (Int, Int)
abilityGain lvl curr
  | lvl == 4  && lvl > curr = (2, 0)
  | lvl == 8  && lvl > curr = (2, 0)
  | lvl == 10 && lvl > curr = (2, 0)
  | lvl == 12 && lvl > curr = (0, 2)
  | lvl == 16 && lvl > curr = (1, 1)
  | lvl == 19 && lvl > curr = (1, 1)
  | otherwise = (0,0)

-- | attacksGain
-- @ Rogue gain ATTACKS w/ lvl
attacksGain :: Int -> Text -> Text
attacksGain lvl attacks
  | lvl == 8  = "2"
  | lvl == 14 = "3"
  | otherwise = attacks

-- | castGain
-- @ Magic Users gain CAST w/ lvl
castGain :: Int -> Text -> Text
castGain lvl cast
  | lvl == 1  = "1d6"
  | lvl == 3  = "2d6"
  | lvl == 5  = "3d6"
  | lvl == 7  = "4d6"
  | lvl == 9  = "5d6"
  | lvl == 11 = "6d6"
  | lvl == 13 = "7d6"
  | lvl == 15 = "8d6"
  | lvl == 17 = "9d6"
  | lvl == 19 = "10d6"
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
  | lvl == 5  = "3"
  | lvl == 10 = "4"
  | lvl == 15 = "5"
  | otherwise = search
