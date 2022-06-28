{-# LANGUAGE OverloadedStrings #-}
{-

Game.Equipment.hs

Game.Equipment is the rules for Armor, Shields, and more...

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Equipment (armorShield) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Kind.Entity
import Game.Rules

-- | armorShield
-- Armor and Shields modify '@' stats
--   1. ArmorClass (AC)
--   2. Weight (WT)
--   3. Weapon WT (WWT)
--   4. ATTACK, SHOOT Damage
armorShield :: Properties -> AssetMap -> Properties
armorShield pProp am = let
  tagLookup :: Text -> [String]
  tagLookup n = let
    tag = Map.findWithDefault "None" n pProp
    in map T.unpack $ T.splitOn ":" $ Map.findWithDefault "I:0:0:" tag descMap
  -- descriptions
  descMap = Map.fromList $
    [ (name, desc) | (_, v) <- Map.toList am,
      let name = Map.findWithDefault "None" "Name" (property v)
          desc = Map.findWithDefault "None" "Description" (property v) ]
  -- melee
  meleeStat = tagLookup "melee"
  (mDam, mWT) = (T.pack (meleeStat!!1), read $ meleeStat!!2) :: (Text, Int)
  -- shoot
  shootStat = tagLookup "shoot"
  (rDam, rWT) = (T.pack (shootStat!!1), read $ shootStat!!2) :: (Text, Int)
  -- shield
  shieldStat = tagLookup "shield"
  (sAC, sWT) = (read $ shieldStat!!1, read $ shieldStat!!2) :: (Int, Int)
  -- armor
  armorStat = tagLookup "armor"
  (aAC, aWT) = (read $ armorStat!!1, read $ armorStat!!2) :: (Int, Int)
  -- Dex modifier
  pDex = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "dex" pProp
  pDR n
    | n > 40 = 0
    | n > 20 && n <= 40 = if pDex > 2 then 2 else pDex
    | otherwise = pDex
  -- ArmorClass (AC)
  pAC = aAC + sAC + pDR aWT
  -- Weight (WT)
  pWT = aWT + sWT + mWT + rWT
  -- '@' Combat Stats
  newProp = Map.fromList [ ("AC", T.pack $ show pAC)
                         , ("WT", T.pack $ show pWT)
                         , ("WWT", T.pack $ show mWT)
                         , ("ATTACK", mDam)
                         , ("SHOOT", rDam)
                         , ("TAGS", gearTags pProp descMap)
                         ]
  in Map.union newProp pProp

-- | @ Tags
-- Unique tags based on @ equipment
gearTags :: Properties -> Properties -> Text
gearTags pProp descMap = let
  tagLookup :: Text -> Text
  tagLookup n = let
    tag = Map.findWithDefault "None" n pProp
    desc = T.splitOn ":" $ Map.findWithDefault ":::" tag descMap
    in desc!!3
  tagReduce :: Text -> Text
  tagReduce xs = T.intercalate "," $ map (T.append "+") $ T.splitOn "," xs
  gear = [ "melee", "shield", "shoot", "armor", "head"
         , "feet", "hands", "jewelry", "neck", "cloak" ]
  in tagReduce $ T.intercalate "," $ map tagLookup gear
