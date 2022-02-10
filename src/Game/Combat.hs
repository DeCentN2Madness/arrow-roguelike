{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Combat (mkCombat) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Arrow.Data (World (..))
import qualified Game.DiceSet as DS
import qualified Game.Entity as GE
import qualified Game.Journal as GJ
import Game.Kind.Entity (Entity(..), EntityKind(..))

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | attack string
attack :: Int -> Int -> String
attack ar dr = if ar >= dr then " hits the " else " misses the "

-- | clamp crits on > 20
clamp :: Int -> Int
clamp n
  | n < 1 = 1
  | n > 20 = 2*n
  | otherwise = n

-- | mkCombat
-- p v m
-- 1. toHit AR >= DR
-- 2. damage D4 + pStr
-- 3. record damage
-- 4. updateWorld
mkCombat :: Int -> Int -> World -> World
mkCombat px mx w = if px == mx
  then w
  else let
    (pEntity, pPos) = GE.getEntityAt px (entityT w)
    (mEntity, mPos) = GE.getEntityAt mx (entityT w)
    -- random seed
    pSeed = (tick w + pHP*pHP) * uncurry (*) pPos :: Int
    -- p attacks
    pProp = property pEntity
    pStr  = read $ Map.findWithDefault "1" "str" pProp :: Int
    pDex  = read $ Map.findWithDefault "1" "dex" pProp :: Int
    pHP   = hitPoint pEntity
    pAR   = clamp $ DS.d20 pSeed + abilityMod pDex
    pDam  = clamp $ DS.d4 pSeed + abilityMod pStr
    -- m defends
    mProp = property mEntity
    mDex  = read $ Map.findWithDefault "1" "dex" mProp :: Int
    mDR   = 10 + abilityMod mDex
    mHP   = hitPoint mEntity
    -- attack
    pAttack = if pAR >= mDR
      then mHP - pDam
      else mHP -- Miss
    mDeath = if pAttack < 1 then "Dead!" else "..."
    pEntry = T.pack $
      show (kind pEntity)
      ++ attack pAR mDR
      ++ show (kind mEntity)
      ++ "! " ++ mDeath
    -- newEntity with damages and deaths
    newEntity = if pAttack < 1
      then GE.insertEntity mx mPos Corpse (entityT w)
      else GE.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }
