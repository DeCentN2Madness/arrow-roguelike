{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Combat (mkCombat) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Engine.Arrow.Data (World (..))
import qualified Game.Actor as GA
import qualified Game.DiceSet as DS
import Game.Kind.Entity (Entity(..), EntityKind(..))

abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | clamp crits on > 20
clamp :: Int -> Int
clamp n
  | n < 1 = 1
  | n > 20 = 2*n
  | otherwise = n

-- | mkCombat
-- 1. toHit D20 + AR > 10 + DR
-- 2. damage D4 + 2
-- 3. record damage
-- 4. updateWorld
mkCombat :: Int -> Int -> World -> World
mkCombat px mx w = if px == mx
  then w
  else let
    pEntity = GA.getEntityAt px (entityT w)
    mEntity = GA.getEntityAt mx (entityT w)
    pPos = coord pEntity
    mPos = coord mEntity
    -- random seed
    pSeed = tick w + uncurry (*) pPos :: Int
    mSeed = tick w + tick w * uncurry (*) mPos :: Int
    -- player
    pProp = prop pEntity
    pDex = read (Map.findWithDefault "1" "dex" pProp) :: Int
    pStr = read $ Map.findWithDefault "1" "str" pProp :: Int
    pDR = 10 + pDex
    pHP = hitPoint pEntity
    pHit = clamp $ DS.d20 pSeed + abilityMod pDex
    pDam = clamp $ DS.d4 pSeed + abilityMod pStr
    -- monster
    mProp = prop mEntity
    mDex = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mStr = read $ Map.findWithDefault "1" "str" mProp :: Int
    mDR = 12 :: Int
    mHP = hitPoint mEntity
    mHit = clamp $ DS.d20 mSeed + abilityMod mDex
    mDam = clamp $ DS.d4 mSeed + abilityMod mStr
    -- attacks
    pAttack = if pHit >= mDR
      then mHP - pDam
      else mHP -- Miss
    mAttack = if mHit >= pDR
      then pHP - mDam
      else pHP -- Miss
    -- journal entry with damages
    pDeath = if mAttack < 1 then "Dead! id=" ++ show px else "..."
    mDeath = if pAttack < 1 then "Dead! id=" ++ show mx else "..."
    pEntry = T.pack $ "Kicks!"
      ++ " pHit="
      ++ show pHit
      ++ ", pDam="
      ++ show pDam
      ++ ", pDR="
      ++ show pDR
      ++ ", pHP="
      ++ show pHP
      ++ ", " ++ pDeath
    mEntry = T.pack $ "Bites!"
      ++ " mHit="
      ++ show mHit
      ++ " mDam="
      ++ show mDam
      ++ ", mDR="
      ++ show mDR
      ++ ", mHP="
      ++ show mHP
      ++ ", " ++ mDeath
    final = if last (journal w) == pEntry
      then journal w
      else journal w ++ [pEntry, mEntry]
    -- entity map with damages and deaths
    -- player is Invulnerable for now
    newEntity = if pAttack < 1
      then GA.insertEntity mx mPos Corpse (entityT w)
      else GA.updateEntity mx pAttack (entityT w)
  in w { entityT = GA.updateEntity px mAttack newEntity
       , journal = final}
