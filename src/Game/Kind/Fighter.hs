{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Fighter.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Fighter where

import Data.Text (Text)

-- | abilityGain
-- @ Primary+8, Secondary+6 stats gain w/ lvl
abilityGain :: Int -> Int -> (Int, Int)
abilityGain lvl curr
  | lvl == 4  && lvl > curr = (2, 0)
  | lvl == 6  && lvl > curr = (2, 0)
  | lvl == 8  && lvl > curr = (2, 0)
  | lvl == 12 && lvl > curr = (0, 2)
  | lvl == 14 && lvl > curr = (0, 2)
  | lvl == 16 && lvl > curr = (1, 1)
  | lvl == 19 && lvl > curr = (1, 1)
  | otherwise = (0,0)

-- | attacksGain
-- @ Fighters gain ATTACKS w/ lvl
attacksGain :: Int -> Text -> Text
attacksGain lvl attacks
  | lvl == 5  = "2"
  | lvl == 11 = "3"
  | lvl == 20 = "4"
  | otherwise = attacks

-- | castGain
-- @ Fighters don't cast
castGain :: Int -> Text -> Text
castGain _ _  = "0"

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
