{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Combat (mkCombat
                   , mkMagicCombat
                   , mkRangeCombat) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Data (World (..))
import qualified Game.DiceSet as DS
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

type AssetMap = EntityMap

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | attack verb
attack :: Int -> Int -> Int -> Text -> Text
attack ar dr dam name = if ar >= dr
  then T.append " hits the " $
  T.append name $ T.pack $ " <" ++ show dam ++ ">"
  else T.append " misses " name

-- | clamp crits on > 20
clamp :: Int -> Int
clamp n
  | n < 1 = 1
  | n > 20 = 2*n
  | otherwise = n

-- | condition of Monster
condition :: Int -> Text
condition hp = let
  dead  = if hp < 1 then ", Dead!" else "..."
  brave = if hp >= 1 && hp <= 5 then ", *Critical* " else ""
  in T.append brave dead

-- | death
-- Inventory drop around the Corpse...
death :: Int -> EntityKind -> AssetMap -> EntityMap -> EntityMap
death mx mEntity am em = let
  mPos   = coord mEntity
  mInv   = inventory mEntity
  mArrow = Map.findWithDefault 0 "Arrow"    mInv
  mItem  = Map.findWithDefault 0 "Item"     mInv
  mMush  = Map.findWithDefault 0 "Mushroom" mInv
  mPot   = Map.findWithDefault 0 "Potion"   mInv
  loc    = scatter mEntity
  -- item mostly Coin
  item
    | mItem   > 0 = GI.mkRandItem loc am
    | mPot    > 0 = GI.mkDropItem "Potion"   loc am
    | mMush   > 0 = GI.mkDropItem "Mushroom" loc am
    | mArrow  > 0 = GI.mkDropItem "Arrow"    loc am
    | otherwise   = GI.mkDropItem "Coin"     loc am
  corpse = GI.mkDropItem "Corpse" mPos am
  newCorpse = GE.updateEntity mx corpse em
  in GI.putDown item newCorpse

-- | misFire
-- Arrows that miss the mark...
misFire :: EntityKind -> AssetMap -> EntityMap -> EntityMap
misFire mEntity am em = let
  loc  = scatter mEntity
  item = GI.mkDropItem "Arrow" loc am
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
    pStr  = read $ T.unpack $ Map.findWithDefault "1" "AR" pProp
    pMod  = read $ T.unpack $ Map.findWithDefault "1" "Proficiency" pProp
    pWeap = Map.findWithDefault "1d4" "ATTACK" pProp
    pAR   = clamp $ DS.d20 pSeed + pStr + pMod
    pDam  = clamp $ weapon pWeap pSeed pStr
    -- mDR, mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mDef  = read $ T.unpack $ Map.findWithDefault "1" "DR" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    mDR   = mAC + mDef
    -- p v m
    pAttack = if pAR >= mDR then mHP - pDam else mHP -- Miss
    -- journal
    pEntry = T.concat [ pName
                      , attack pAR mDR pDam mName
                      , condition pAttack ]
    -- newEntity with damages and deaths and Exp awards
    newEntity = if pAttack < 1
      then death mx mEntity (assetT w) $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | mkMagicCombat
-- TODO Magic Skill, Ball, Bolt; Cone, and effects...
mkMagicCombat :: Int -> Int -> World -> World
mkMagicCombat px mx w = if px == mx
  then w
  else let
    (pEntity, pPos) = GE.getEntityAt px (entityT w)
    (mEntity, mPos) = GE.getEntityAt mx (entityT w)
    -- random seed
    pSeed = tick w + (uncurry (*) mPos * uncurry (*) pPos) :: Int
    -- pAR, pDam, pMod
    pProp = property pEntity
    pName = Map.findWithDefault "P" "Name" pProp
    pInt  = read $ T.unpack $ Map.findWithDefault "1" "MR" pProp
    pMod  = read $ T.unpack $ Map.findWithDefault "1" "Proficiency" pProp
    pWeap = Map.findWithDefault "1d4" "ATTACK" pProp
    pAR   = clamp $ DS.d20 pSeed + pInt + pMod
    pDam  = clamp $ weapon pWeap pSeed pInt
    -- mDR,  mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mDef  = read $ T.unpack $ Map.findWithDefault "1" "DR" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    mDR   = mAC + mDef
    -- p v m
    pAttack = if pAR >= mDR then mHP - pDam else mHP -- Miss
    -- journal
    pEntry = T.concat [ pName
                      , shootM pAR mDR pDam mName
                      , condition pAttack ]
    -- newEntity with damages and deaths and Exp awards
    newEntity = if pAttack < 1
      then death mx mEntity (assetT w) $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | mkRangeCombat
-- Throw, shoot...
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
    pDex  = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "dex" pProp
    pMod  = read $ T.unpack $ Map.findWithDefault "1" "Proficiency" pProp
    pWeap = Map.findWithDefault "1d4" "SHOOT" pProp
    pAR   = clamp $ DS.d20 pSeed + pDex + pMod
    pDam  = clamp $ weapon pWeap pSeed pDex
    -- mDR,  mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mDef  = read $ T.unpack $ Map.findWithDefault "1" "DR" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    mDR   = mAC + mDef
    -- p v m
    pAttack = if pAR >= mDR then mHP - pDam else mHP -- Miss
    -- misfire
    shotEntity = if pAR < mDR
      then misFire mEntity (assetT w) (entityT w)
      else entityT w
    -- journal
    pEntry = T.concat [ pName
                      , shoot pAR mDR pDam mName
                      , condition pAttack ]
    -- newEntity with damages and deaths and Exp awards
    newEntity = if pAttack < 1
      then death mx mEntity (assetT w) $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack shotEntity
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | nth safe chooser
nth :: Int -> [(Int, Int)] -> (Int, Int)
nth _ []     = (0, 0)
nth 1 (x:_)  = x
nth n (_:xs) = nth (n-1) xs

-- | scatter
scatter :: EntityKind -> (Int, Int)
scatter mEntity = let
  mPos     = coord mEntity
  -- random around the target
  seed     = 1 + uncurry (*) mPos
  missList = moveT mEntity ++ [mPos]
  sz       = length missList - 1
  missRoll = head $ DS.rollList 1 (fromIntegral sz) seed
  in nth missRoll missList

-- | shoot verb
shoot :: Int -> Int -> Int -> Text -> Text
shoot ar dr dam name = if ar >= dr
  then T.append " shoots -Arrow~ at " $
  T.append name $ T.pack $ " <" ++ show dam ++ ">"
  else T.append " ~Arrow~ misses the " name

-- | shoot verb
shootM :: Int -> Int -> Int -> Text -> Text
shootM ar dr dam name = if ar >= dr
  then T.append " casts -Spell- at " $
  T.append name $ T.pack $ " <" ++ show dam ++ ">"
  else T.append " -Spell- misses the " name

-- | weapon
-- weapon dice
weapon :: Text -> Int -> Int -> Int
weapon dice seed bonus = let
  roll = case dice of
    "1d4" -> DS.d4 seed
    "1d6" -> DS.d6 seed
    "1d8" -> DS.d8 seed
    "1d10" -> DS.d10 seed
    "1d12" -> DS.d12 seed
    "2d4" -> DS.d4 seed + DS.d4 (seed+1)
    "2d6" -> DS.d6 seed + DS.d6 (seed+1)
    _     -> DS.d4 seed
  in roll + bonus
