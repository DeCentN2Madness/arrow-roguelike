{-# LANGUAGE OverloadedStrings #-}
{-

Game.Combat.hs

P vs. M
  0. Check Weapon WT (WWT) and Weight (WT) for Encumbered and Finesse...
     a. if WWT and/or WT is Heavy/Encumbered lose Proficiency
     b. Shooters and Casters must use light weapons (<5 lbs)...
  1. Roll AR on D20
  2. if pAR >= mAC then pDam else Miss
     a. Melee: pDam is Weapon + pStr
     b. Throw: pDam is Weapon + pDex
     c. Cast:  pDam is Weapon + pInt
  3. mHP is recorded
  4. updateWorld with misfires, Deaths and Corpses...

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
condition :: Int -> Int -> Text
condition hp xp = let
  dead  = if hp < 1
    then T.append " Dead!" $
    T.pack $ " You gain " ++ show xp ++ " experience."
    else "."
  brave = if hp >= 1 && hp <= 5 then " *Critical*" else ""
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
-- Melee...
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
    -- Finesse?
    pStr  = read $ T.unpack $ Map.findWithDefault "1" "str" pProp :: Int
    pDex  = read $ T.unpack $ Map.findWithDefault "1" "dex" pProp :: Int
    pWWT  = read $ T.unpack $ Map.findWithDefault "3" "WWT" pProp :: Int
    pStat = if pWWT < 3 then abilityMod pDex else abilityMod pStr
    pWeap = Map.findWithDefault "1d4" "ATTACK" pProp
    -- Encumbered?
    pWT   = read $ T.unpack $ Map.findWithDefault "0" "WT" pProp :: Int
    pMod  = read $ T.unpack $ Map.findWithDefault "0" "Proficiency" pProp
    pEnc  = if pWT > 5 * pStr then 0 else pMod
    -- ATTACK roll
    pAR   = clamp $ DS.d20 pSeed + pStat + pEnc
    pDam  = clamp $ weapon pWeap pSeed pStat
    -- mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    -- p v m
    pAttack = if pAR >= mAC then mHP - pDam else mHP -- Miss
    -- journal
    pEntry = T.concat [ pName
                      , attack pAR mAC pDam mName
                      , condition pAttack mExp ]
    -- newEntity with damages and deaths and Exp awards
    newEntity = if pAttack < 1
      then death mx mEntity (assetT w) $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack (entityT w)
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | mkMagicCombat
-- Cast, Chant, Magic Combat...
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
    -- MAGIC
    pInt  = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "int" pProp
    pWeap = Map.findWithDefault "1d4" "ATTACK" pProp
    -- Encumbered, Heavy weapons?
    pStr  = read $ T.unpack $ Map.findWithDefault "1" "str" pProp :: Int
    pMod  = read $ T.unpack $ Map.findWithDefault "0" "Proficiency" pProp
    pWT   = read $ T.unpack $ Map.findWithDefault "0" "WT" pProp  :: Int
    pWWT  = read $ T.unpack $ Map.findWithDefault "0" "WWT" pProp :: Int
    pEnc  = if pWT < 5 * pStr && pWWT < 5 then pMod else 0
    -- CAST roll
    pAR   = clamp $ DS.d20 pSeed + pInt + pEnc
    pDam  = clamp $ weapon pWeap pSeed pInt
    -- mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    -- p v m
    pAttack = if pAR >= mAC then mHP - pDam else mHP -- Miss
    -- miscast
    shotEntity = if pAR < mAC
      then misFire mEntity (assetT w) (entityT w)
      else entityT w
    -- journal
    pEntry = T.concat [ pName
                      , shootM pAR mAC pDam mName
                      , condition pAttack mExp ]
    -- newEntity with damages and deaths and Exp awards
    newEntity = if pAttack < 1
      then death mx mEntity (assetT w) $ GP.updatePlayerXP mExp (entityT w)
      else GE.updateEntityHp mx pAttack shotEntity
  in w { entityT  = newEntity
       , journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | mkRangeCombat
-- Shoot, Throw, Ranged Combat...
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
    -- THROW
    pDex  = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "dex" pProp
    pWeap = Map.findWithDefault "1d4" "SHOOT" pProp
    -- Encumbered, Heavy weapons?
    pStr  = read $ T.unpack $ Map.findWithDefault "1" "str" pProp :: Int
    pMod  = read $ T.unpack $ Map.findWithDefault "0" "Proficiency" pProp
    pWT   = read $ T.unpack $ Map.findWithDefault "0" "WT" pProp  :: Int
    pWWT  = read $ T.unpack $ Map.findWithDefault "0" "WWT" pProp :: Int
    pEnc  = if pWT < 5 * pStr && pWWT < 5 then pMod else 0
    -- SHOOT roll
    pAR   = clamp $ DS.d20 pSeed + pDex + pEnc
    pDam  = clamp $ weapon pWeap pSeed pEnc
    -- mAC
    mProp = property mEntity
    mName = Map.findWithDefault "M" "Name" mProp
    mAC   = read $ T.unpack $ Map.findWithDefault "1" "AC" mProp
    mHP   = eHP mEntity
    mExp  = eXP mEntity
    -- p v m
    pAttack = if pAR >= mAC then mHP - pDam else mHP -- Miss
    -- misfire
    shotEntity = if pAR < mAC
      then misFire mEntity (assetT w) (entityT w)
      else entityT w
    -- journal
    pEntry = T.concat [ pName
                      , shoot pAR mAC pDam mName
                      , condition pAttack mExp ]
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
  missList = moveT mEntity
  missRoll = head $ DS.rollList 1 (fromIntegral $ length missList) seed
  in nth missRoll missList

-- | shoot verb
shoot :: Int -> Int -> Int -> Text -> Text
shoot ar dr dam name = if ar >= dr
  then T.append " shoots ~Arrow~ at " $
  T.append name $ T.pack $ " <" ++ show dam ++ ">"
  else T.append " ~Arrow~ misses the " name

-- | shoot verb
shootM :: Int -> Int -> Int -> Text -> Text
shootM ar dr dam name = if ar >= dr
  then T.append " casts -Spell- at " $
  T.append name $ T.pack $ " <" ++ show dam ++ ">"
  else T.append " -Spell- misses the " name

-- | weapon
-- Damage roll for Weapons...
-- Example: 1d4+1
weapon :: Text -> Int -> Int -> Int
weapon dice seed bonus = let
  (wDam, wMod) = T.breakOn "+" dice
  roll = case wDam of
    "1d1" -> 1
    "1d4" -> DS.d4 seed
    "1d6" -> DS.d6 seed
    "1d8" -> DS.d8 seed
    "1d10" -> DS.d10 seed
    "1d12" -> DS.d12 seed
    "2d4" -> DS.d4 seed + DS.d4 (seed+1)
    "2d6" -> DS.d6 seed + DS.d6 (seed+1)
    _     -> DS.d4 seed
  wBonus n
    | n == "+1" = 1
    | n == "+2" = 2
    | n == "+3" = 3
    | n == "+4" = 4
    | otherwise = 0
  in roll + bonus + wBonus wMod
