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
--   5. TAGS from Equipment
armorShield :: Properties -> AssetMap -> Properties
armorShield pProp am = let
  -- clampAC
  clampAC n = if n < 10 then 10 else n
  -- pDR
  pDex = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "dex" pProp
  pDR n
    | n > 40 = 0
    | n > 20 && n <= 40 = if pDex > 2 then 2 else pDex
    | otherwise = pDex
  -- Equipment
  (meleeDam, meleeWT)  = tagLookup "melee" pProp am
  (shootDam, shootWT)  = tagLookup "shoot" pProp am
  (armorAC, armorWT)   = tagLookup "armor" pProp am
  (_, cloakWT)         = tagLookup "cloak" pProp am
  (shieldAC, shieldWT) = tagLookup "shield" pProp am
  (_, headWT)          = tagLookup "head" pProp am
  (_, handWT)          = tagLookup "hands" pProp am
  (_, feetWT)          = tagLookup "feet" pProp am
  -- ArmorClass (AC)
  pAC = sum [ clampAC $ read $ T.unpack armorAC
            , read $ T.unpack shieldAC
            , pDR armorWT ]
  -- Weight (WT)
  pWT = sum [ armorWT, shieldWT, meleeWT, shootWT, headWT, feetWT, handWT, cloakWT ]
  -- '@' Combat Properties
  newProp = Map.fromList [ ("AC", T.pack $ show pAC)
                         , ("WT", T.pack $ show pWT)
                         , ("WWT", T.pack $ show meleeWT)
                         , ("ATTACK", meleeDam)
                         , ("SHOOT", shootDam)
                         , ("TAGS", gearTags pProp am) ]
  in Map.union newProp pProp

-- | Descriptions
descriptions :: AssetMap -> Properties
descriptions am = Map.fromList $
  [ (name, desc) | (_, v) <- Map.toList am,
    let name = Map.findWithDefault "None" "Name" (property v)
        desc = Map.findWithDefault "None" "Description" (property v) ]

-- | @ Tags
-- Unique tags based on @ equipment
gearTags :: Properties -> AssetMap -> Text
gearTags pProp am = let
  gear = [ "melee", "shoot", "jewelry", "neck", "armor", "cloak"
         , "shield", "head", "hands", "feet" ]
  tags = [ v | x <- gear, let v = tagLookup' x pProp am ]
  in tagFormat $ T.intercalate "," tags

-- | tagFormat
tagFormat :: Text -> Text
tagFormat xs = T.intercalate "," $ map (T.append "+") $ T.splitOn "," xs

-- | tagLookup equipment values
tagLookup :: Text -> Properties -> AssetMap -> (Text, Int)
tagLookup n pProp am = let
  descMap = descriptions am
  tag = Map.findWithDefault "None" n pProp
  tags = map T.unpack $ T.splitOn ":" $ Map.findWithDefault ":0:0:" tag descMap
  in (T.pack $ tags!!1, read $ tags!!2)

-- | tagLookup equipment propeties
tagLookup' :: Text -> Properties -> AssetMap -> Text
tagLookup' n pProp am = let
  descMap = descriptions am
  tag = Map.findWithDefault "None" n pProp
  desc = T.splitOn ":" $ Map.findWithDefault ":::" tag descMap
  in desc!!3
