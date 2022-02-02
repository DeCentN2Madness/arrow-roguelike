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
import Game.Kind.Entity (EntityKind(..))

abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

clamp :: Int -> Int
clamp r
  | r < 1 = 1
  | r > 20 = 20
  | otherwise = r

-- | mkCombat
-- TODO pass in better entropy for DS.roll
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
    pxy = coord pEntity
    mxy = coord mEntity
    pSeed = tick w + uncurry (*) mxy:: Int -- Seed, compute
    mSeed = tick w + tick w * uncurry (*) pxy:: Int -- Seed, compute
    -- player
    pProp = prop pEntity
    pDex = read (Map.findWithDefault "1" "dex" pProp) :: Int
    pStr = read $ Map.findWithDefault "1" "str" pProp :: Int
    pDR = 10 + pDex
    pHP = hitPoint pEntity
    pHit = DS.d20 pSeed + abilityMod pDex
    pDam = clamp $ DS.d4 pSeed + abilityMod pStr
    -- monster
    mProp = prop mEntity
    mDex = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mStr = read $ Map.findWithDefault "1" "str" mProp :: Int
    mDR = 12 :: Int
    mHP = hitPoint mEntity
    mHit = DS.d20 mSeed + abilityMod mDex
    mDam = clamp $ DS.d4 mSeed + abilityMod pStr
    -- attacks
    pAttack = if pHit >= mDR
      then mHP - pDam
      else mHP -- Miss
    mAttack = if mHit >= pDR
      then pHP - mDam
      else pHP -- Miss
    -- entityMap with damages
    pEntry = T.pack $ "Kicks!"
      ++ " pHit="
      ++ show pHit
      ++ ", pDam="
      ++ show pDam
      ++ ", pDex="
      ++ show pDex
      ++ ", pStr="
      ++ show pStr
      ++ ", pDR="
      ++ show pDR
      ++ ", pHP="
      ++ show pHP
    mEntry = T.pack $ "Bites!"
      ++ " mHit="
      ++ show mHit
      ++ " mDam="
      ++ show mDam
      ++ ", mDex="
      ++ show mDex
      ++ ", mStr="
      ++ show mStr
      ++ ", mDR="
      ++ show mDR
      ++ ", mHP="
      ++ show mHP
    mDeath = if pAttack < 1 then T.pack $ "Dead! id=" ++ show mx else "..."
    final = if last (journal w) == pEntry
      then journal w
      else journal w ++ [pEntry, mEntry, mDeath]
  in w { entityT = GA.updateEntity px mAttack $
         GA.updateEntity mx pAttack (entityT w)
       , journal = final}
