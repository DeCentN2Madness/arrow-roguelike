{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Combat (mkCombat) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Arrow.Data (World (..))
import qualified Game.Actor as GA
import qualified Game.DiceSet as DS
import qualified Game.Journal as GJ
import Game.Kind.Entity (Entity(..), EntityKind(..))

abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

attack :: Int -> Int -> String
attack ar dr = if ar >= dr then " hits the " else " misses the "

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
    (pEntity, pPos) = GA.getEntityAt px (entityT w)
    (mEntity, mPos) = GA.getEntityAt mx (entityT w)
    -- random seed
    pSeed = (tick w + pHP*pHP) * uncurry (*) pPos :: Int
    mSeed = (tick w + mHP*mHP) * uncurry (*) mPos :: Int
    -- player
    pProp = property pEntity
    pStr = read $ Map.findWithDefault "1" "str" pProp :: Int
    pDex = read $ Map.findWithDefault "1" "dex" pProp :: Int
    pDR = 10 + abilityMod pDex
    pHP = hitPoint pEntity
    pAR = clamp $ DS.d20 pSeed + abilityMod pDex
    pDam = clamp $ DS.d4 pSeed + abilityMod pStr
    -- monster
    mProp = property mEntity
    mStr = read $ Map.findWithDefault "1" "str" mProp :: Int
    mDex = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mDR = 12 :: Int
    mHP = hitPoint mEntity
    mAR = clamp $ DS.d20 mSeed + abilityMod mDex
    mDam = clamp $ DS.d4 mSeed + abilityMod mStr
    -- attacks
    pAttack = if pAR >= mDR
      then mHP - pDam
      else mHP -- Miss
    mAttack = if mAR >= pDR
      then pHP - mDam
      else pHP -- Miss
    -- journal entry with damages
    pDeath = if mAttack < 1 then "Dead!" else "..."
    mDeath = if pAttack < 1 then "Dead!" else "..."
    pEntry = T.pack $
      show (kind pEntity)
      ++ attack pAR mDR
      ++ show (kind mEntity)
      ++ "! " ++ mDeath
    mEntry = T.pack $
      show (kind mEntity)
      ++ attack mAR pDR
      ++ show (kind pEntity)
      ++ "! " ++ pDeath
    -- entity map with damages and deaths
    -- player is Invulnerable for now
    newEntity = if pAttack < 1
      then GA.insertEntity mx mPos Corpse (entityT w)
      else GA.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = GA.updateEntityHp px mAttack newEntity
       , journalT = GJ.updateJournal [pEntry, mEntry] (journalT w) }
