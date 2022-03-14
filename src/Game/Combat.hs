{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Combat (mkCombat, mkRangeCombat) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Data (World (..))
import qualified Game.DiceSet as DS
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity (Entity(..), EntityKind(..))
import qualified Game.Player as GP

type AssetMap = EntityMap

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | attack string
attack :: Int -> Int -> Text
attack ar dr = if ar >= dr
  then T.pack " hits the "
  else " attack misses the "

-- | clamp crits on > 20
clamp :: Int -> Int
clamp n
  | n < 1 = 1
  | n > 20 = 2*n
  | otherwise = n

condition :: Int -> Text
condition hp = let
  dead  = if hp < 1 then "Dead!" else "..."
  brave = if hp >= 1 && hp <= 5 then "*Critical* " else ""
  in T.append brave dead

-- | Arrows that miss the mark
misFire :: EntityKind -> AssetMap -> EntityMap -> EntityMap
misFire mEntity am em = let
  mPos = coord mEntity
  seed = 1 + uncurry (*) mPos
  missList = moveT mEntity
  sz = length missList - 1
  missRoll = head $ DS.rollList 1 (fromIntegral sz) seed
  location = missList !! missRoll
  item = GI.mkItem "Unknown" location am
  in GI.putDown item em

-- | mkCombat
-- p v m
-- 1. if pAR >= mDR then pDam
-- 2. pDam is Weapon + pStr
-- 3. mHP is recorded
-- 4. updateWorld with deaths and corpses
mkCombat :: Int -> Int -> World -> World
mkCombat px mx w = if px == mx
  then w
  else let
    (pEntity, pPos) = GE.getEntityAt px (entityT w)
    (mEntity, mPos) = GE.getEntityAt mx (entityT w)
    -- random seed
    pSeed = tick w + (uncurry (*) mPos * uncurry (*) pPos) :: Int
    -- pAR, pDam, pMod
    pProp = property pEntity
    pName = Map.findWithDefault "P" "Name" pProp
    pStr  = read $ Map.findWithDefault "1" "str" pProp :: Int
    pDex  = read $ Map.findWithDefault "1" "dex" pProp :: Int
    pMod  = read $ Map.findWithDefault "1" "Proficiency" pProp :: Int
    pAR   = clamp $ DS.d20 pSeed + abilityMod pDex + pMod
    pDam  = clamp $ DS.d6  pSeed + abilityMod pStr + pMod
    -- mDR
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mDex  = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    mDR   = 10 + abilityMod mDex
    -- p v m
    pAttack = if pAR >= mDR
      then mHP - pDam
      else mHP -- Miss
    -- journal
    pEntry = T.concat [ T.pack pName
                    , attack pAR mDR
                    , T.pack mName
                    , T.pack ", "
                    , condition pAttack ]
    -- newEntity with damages and deaths
    -- Exp Award
    newEntity = if pAttack < 1
      then GE.insertEntity mx mPos Corpse $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | mkRangeCombat
-- Throw, shoot, cast...
mkRangeCombat :: Int -> Int -> World -> World
mkRangeCombat px mx w = if px == mx
  then w
  else let
    (pEntity, pPos) = GE.getEntityAt px (entityT w)
    (mEntity, mPos) = GE.getEntityAt mx (entityT w)
    -- random seed
    pSeed = tick w + (uncurry (*) mPos * uncurry (*) pPos) :: Int
    -- pAR, pDam, pMod
    pProp = property pEntity
    pName = Map.findWithDefault "P" "Name" pProp
    pDex  = read $ Map.findWithDefault "1" "dex" pProp :: Int
    pMod  = read $ Map.findWithDefault "1" "Proficiency" pProp :: Int
    pAR   = clamp $ DS.d20 pSeed + abilityMod pDex + pMod
    pDam  = clamp $ DS.d4  pSeed + abilityMod pDex + pMod
    -- mDR,  mMod
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mDex  = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    mDR   = 10 + abilityMod mDex
    -- p v m
    pAttack = if pAR >= mDR
      then mHP - pDam
      else mHP -- Miss
    -- misfire
    shotEntity = if pAR < mDR
      then misFire mEntity (assetT w) (entityT w)
      else entityT w
    -- journal
    pEntry = T.concat [ T.pack pName
                    , shoot pAR mDR
                    , T.pack mName
                    , T.pack ", "
                    , condition pAttack ]
    -- newEntity with damages and deaths
    -- Exp Award
    newEntity = if pAttack < 1
      then GE.insertEntity mx mPos Corpse $ GP.updatePlayerXP mExp shotEntity
      else GE.updateEntityHp mx pAttack shotEntity
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | shoot string
shoot :: Int -> Int -> Text
shoot ar dr = if ar >= dr
  then T.pack " shoots the "
  else " shot misses the "
