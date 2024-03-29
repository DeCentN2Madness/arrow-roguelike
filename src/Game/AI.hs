{-# LANGUAGE OverloadedStrings #-}
{-

Game.AI.hs

Game.AI.hs is the actions for the Monster 'M' in the Game.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (aiAction) where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Arrow.Data (World(..))
import qualified Game.Combat as GC
import qualified Game.Compass as C
import qualified Game.DiceSet as DS
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity
import qualified Game.Player as GP
import Game.Rules

data AI
  = Attack
  | Cast
  | Drink
  | Eat
  | Get
  | Move
  | Rest
  | Summon
  | Throw
  | Wait
  deriving (Show, Eq)

-- | aiAction
-- AI actions for Monsters based on goal, HP, position...
aiAction :: [(Int, EntityKind)] -> World -> World
aiAction [] w = w
aiAction ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then aiAction xs w
  else let
  (_, pPos) = GP.getPlayer (entityT w)
  -- monster properties
  mHp = eHP mEntity
  mMaxHp = eMaxHP mEntity
  mMp = eMP mEntity
  mInv = inventory mEntity
  mArrow = Map.findWithDefault 0 "Arrow" mInv
  mMush = Map.findWithDefault 0 "Mushroom" mInv
  mPot = Map.findWithDefault 0 "Potion" mInv
  mItems = GE.getEntityBy mPos (entityT w)
  mPos = coord mEntity
  -- Properties
  (mInt, _) = abilityLookup "int" mEntity
  mSummon = propertyLookup "SUMMON" mEntity
  mSpawn = spawn mEntity
  -- action
  action
    | C.adjacent mPos pPos = Attack
    | (mMp > 5) && (mSummon /= "None") && (C.chessDist mPos pPos <= 5) = Summon
    | (mMp > 0)    && (C.chessDist mPos pPos <= 4) = Cast
    | (mArrow > 0) && (C.chessDist mPos pPos <= 4) = Throw
    | (mMush > 0)  && (mMaxHp - mHp > 5) = Eat
    | (mPot > 0)   && (mMaxHp - mHp > 10) = Drink
    | (mHp <= 5)   && C.adjacent mPos mSpawn = Rest
    | (mInt > 6)   && any (\(i, _) -> kind i == Coin) mItems = Get
    | (mInt < 6)   && any (\(i, _) -> kind i == Mushroom) mItems = Get
    | (mInt > 9)   && any (\(i, _) -> kind i == Potion) mItems = Get
    | (mHp > 0) = Move
    | otherwise = Wait
  world = case action of
    Attack -> GC.mkCombat mx 0 w
    Cast   -> monsterCast mx mEntity w
    Drink  -> monsterDrink mx mEntity w
    Eat    -> monsterEat mx mEntity w
    Get    -> monsterGet mx mEntity w
    Move   -> pathFinder mx mEntity w
    Rest   -> monsterHeal mx mEntity w
    Summon -> monsterSummon mx mEntity w
    Throw  -> monsterThrow mx mEntity w
    Wait   -> w
  -- newWorld w/ action
  in aiAction xs world

-- | monsterCast
-- M cast...
monsterCast :: Int -> EntityKind -> World -> World
monsterCast mx mEntity w = let
  mPos = coord mEntity
  mMana = eMP mEntity
  mProp = property mEntity
  mName = Map.findWithDefault "M" "Name" mProp
  mTarget = mPos `elem` fovT w
  newMonster = if mMana > 0 && mTarget
    then mEntity { eMP = if mMana - 1 > 0 then 0 else mMana - 1 }
    else mEntity
  entry = if mMana > 0 && mTarget
    then T.append mName " Casts a Spell.."
    else "..."
  -- throwWorld
  throwWorld = w { entityT  = GE.updateEntity mx newMonster (entityT w)
                , journalT = GJ.updateJournal [entry] (journalT w) }
  -- FoV check
  world = if mMana > 0 && mTarget
    then GC.mkMagicCombat mx 0 throwWorld
    else w
  in world

-- | monsterDrink
-- M drinks...
monsterDrink :: Int -> EntityKind -> World -> World
monsterDrink mx mEntity w = let
  seed = tick w
  mInv = inventory mEntity
  mPot = Map.findWithDefault 0 "Potion" mInv
  hRoll = DS.d4 seed + DS.d4 (seed+1)
  mRoll = DS.d4 (seed+2) + DS.d4 (seed+3)
  heal = eHP mEntity + hDelta
  mana = eMP mEntity + mDelta
  hDelta = hRoll + mCon
  mDelta = mRoll + mWis
  mHp = if heal > mMaxHp then mMaxHp else heal
  mMp = if mana > mMaxMp then mMaxMp else mana
  mMaxHp = eMaxHP mEntity
  mMaxMp = eMaxMP mEntity
  -- Properties
  mName     = Map.findWithDefault "M" "Name" (property mEntity)
  (_, mCon) = abilityLookup "con" mEntity
  (_, mWis) = abilityLookup "wis" mEntity
  newMonster = if mPot > 0
    then mEntity { inventory = Map.insert "Potion" (mPot-1) mInv
                 , eHP = mHp, eMP = mMp }
    else mEntity
  entry = if mPot > 0
    then T.concat [ mName
                 , " is Thirsty:"
                 , abilityResult2 hDelta mDelta hRoll mRoll mCon mWis 0 ]
    else "..."
  in w { entityT  = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterEat
-- M eats...
monsterEat :: Int -> EntityKind -> World -> World
monsterEat mx mEntity w = let
  seed = tick w
  mInv = inventory mEntity
  mMush = Map.findWithDefault 0 "Mushroom" mInv
  hRoll = DS.d8 seed
  heal = eHP mEntity + hDelta
  hDelta = hRoll + mCon
  mHp = if heal > mMaxHp then mMaxHp else heal
  mMaxHp = eMaxHP mEntity
  -- Properties
  mName = Map.findWithDefault "M" "Name" (property mEntity)
  (_, mCon) = abilityLookup "con" mEntity
  newMonster = if mMush > 0
    then mEntity { inventory = Map.insert "Mushroom" (mMush-1) mInv, eHP = mHp }
    else mEntity
  entry = if mMush > 0
    then T.concat [ mName
                  , " is Hungry:"
                  , abilityResult hDelta hRoll mCon 0 ]
    else "..."
  in w { entityT  = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterGet
-- M loves Coin, M beast loves Mushroom...
monsterGet :: Int -> EntityKind -> World -> World
monsterGet mx mEntity w = let
  mPos = coord mEntity
  items = GE.getEntityBy mPos (entityT w)
  mName = Map.findWithDefault "M" "Name" (property mEntity)
  pickedItems = GI.checkPickUp (inventory newMonster) (inventory mEntity)
  newMonster = if not (null items)
    then GI.pickUp items mEntity
    else mEntity
  newEntity = if pickedItems /= Map.empty
    then GI.emptyBy mPos items (entityT w)
    else entityT w
  entry = if pickedItems /= Map.empty
    then T.append mName " Get Coin!"
    else "..."
  in w { entityT  = GE.updateEntity mx newMonster newEntity
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterHeal
-- M heals slowly...
monsterHeal :: Int -> EntityKind -> World -> World
monsterHeal mx mEntity w = let
  heal = eHP mEntity + 1
  mHp = if heal > mMaxHp then mMaxHp else heal
  mMaxHp = eMaxHP mEntity
  mName = Map.findWithDefault "M" "Name" (property mEntity)
  entry = if mHp <= 5
    then T.append mName " is *Hurting*..."
    else "..."
  in w { entityT  = GE.updateEntityHp mx mHp (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterSummon
-- M adds Friends...
monsterSummon :: Int -> EntityKind -> World -> World
monsterSummon mx mEntity w = let
  mMove = moveT mEntity
  mMana = eMP mEntity - 1
  mMaxMP = eMaxMP mEntity
  -- Properties
  mProp = property mEntity
  mName = Map.findWithDefault "M" "Name" mProp
  mSummon = Map.findWithDefault "Mouse" "SUMMON" mProp
  newMonster = if mMana > 0 && mMaxMP > 0 && not (null mMove)
    then mEntity { eMP =  mMana }
    else mEntity
  entry = if mMana > 0 && mMaxMP > 0 && not (null mMove)
    then T.concat [ mName, " Summons a ", mSummon, "!" ]
    else "No Summon..."
  summonWorld = if mMana > 0 && mMaxMP > 0 && not (null mMove)
    then let
    item = GI.mkDropItem mSummon (head mMove) (assetT w)
    in GI.putDown item $ GE.updateEntity mx newMonster (entityT w)
    else entityT w
  in w { entityT  = summonWorld
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterThrow
-- M shoots...
monsterThrow :: Int -> EntityKind -> World -> World
monsterThrow mx mEntity w = let
  mInv = inventory mEntity
  mArrow = Map.findWithDefault 0 "Arrow" mInv
  mPos = coord mEntity
  -- Properties
  mProp = property mEntity
  mName = Map.findWithDefault "M" "Name" mProp
  mVerb = Map.findWithDefault "shoots..." "Throw" mProp
  mTarget = mPos `elem` fovT w
  newMonster = if mArrow > 0 && mTarget
    then mEntity { inventory = Map.insert "Arrow" (mArrow-1) mInv }
    else mEntity
  entry = if mArrow > 0 && mTarget
    then T.append mName $ T.append " " mVerb
    else "..."
  -- throwWorld
  throwWorld = w { entityT  = GE.updateEntity mx newMonster (entityT w)
                , journalT = GJ.updateJournal [entry] (journalT w) }
  -- FoV check
  world = if mArrow > 0 && mTarget
    then GC.mkRangeCombat mx 0 throwWorld
    else w
  in world

-- | pathFinder
-- 1. Distance from goal
-- 2. Decide by distance
--    a. 4 is Vision
--    b. 7 is Hear
-- 3. Check blockList
-- 4. Update move
pathFinder :: Int -> EntityKind -> World -> World
pathFinder mx mEntity w = let
  coordF :: [(Int, Int)] -> [(Int, Int)]
  coordF = filter (`notElem` [ xy | (_, xy) <- GE.fromBlock (entityT w) ] )
  (_, pPos) = GP.getPlayer (entityT w)
  -- flee goal if *critical* eHP
  mGoal = if eHP mEntity <= 5 then mSpawn else pPos
  mPos = coord mEntity
  mSpawn = spawn mEntity
  -- M move based on goal
  distList = [ (d, xy) | xy <- moveT mEntity, let d = C.chessDist mGoal xy ]
  moveList = coordF $
    [ xy | (d, pos) <- sort distList,
      let xy = if C.adjacent mPos mGoal || d >= 7 then mPos else pos ]
  move = if null moveList then mPos else head moveList
  in w { entityT = GE.updateEntityPos mx move (entityT w) }
