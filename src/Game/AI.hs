{-# LANGUAGE OverloadedStrings #-}
{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (aiAction) where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Arrow.Data (World(..))
import qualified Engine.Arrow.Compass as EAC
import qualified Game.Combat as GC
import qualified Game.Entity as GE
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

data AI
  = Attack
  | Eat
  | Drink
  | Move
  | Rest
  | Throw
  deriving (Show, Eq)

-- | aiAction
-- AI actions based on goal, HP, position...
-- TODO more actions, like throw, use items...
aiAction :: [(Int, EntityKind)] -> World -> World
aiAction [] w = w
aiAction ((mx, mEntity):xs) w = if mx == 0 || not (block mEntity)
  then aiAction xs w
  else let
  (_, pPos) = GP.getPlayer (entityT w)
  -- monster properties
  mHp    = eHP mEntity
  mMaxHp = eMaxHP mEntity
  mInv   = inventory mEntity
  mArrow = Map.findWithDefault 0 "Arrow" mInv
  mMush  = Map.findWithDefault 0 "Mushroom" mInv
  mPot   = Map.findWithDefault 0 "Potion" mInv
  mPos   = coord mEntity
  mProp  = property mEntity
  mSpawn = read $ Map.findWithDefault (show mPos) "spawn" mProp :: (Int, Int)
  -- action
  action
    | EAC.adjacent pPos mPos = Attack
    | (mHp <= 5)   && EAC.adjacent mSpawn mPos = Rest
    | (mArrow > 0) && (EAC.chessDist mPos pPos <= 4) = Throw
    | (mMush  > 0) && (mMaxHp `div` mHp > 2) = Eat
    | (mPot  > 0)  && (mMaxHp `div` mHp > 3) = Drink
    | otherwise = Move
  world = case action of
    Attack -> GC.mkCombat  mx 0 w
    Eat    -> monsterEat   mx mEntity w
    Drink  -> monsterDrink mx mEntity w
    Move   -> pathFinder   mx mEntity w
    Rest   -> monsterHeal  mx mEntity w
    Throw  -> monsterThrow mx mEntity w
  -- newWorld w/ action
  in aiAction xs world

-- | monsterDrink
-- M drinks...
monsterDrink :: Int -> EntityKind -> World -> World
monsterDrink mx mEntity w = let
  mInv       = inventory mEntity
  mPot      = Map.findWithDefault 0 "Potion" mInv
  newMonster = if mPot > 0
    then let
    heal = eHP mEntity + 5
    in mEntity { inventory = Map.insert "Potion" (mPot-1) mInv
               , eHP = if heal > eMaxHP mEntity then eMaxHP mEntity else heal }
    else mEntity
  entry = if mPot > 0
    then T.pack "Something is Thirsty..."
    else T.pack "..."
  in w { entityT = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterEat
-- M eats...
monsterEat :: Int -> EntityKind -> World -> World
monsterEat mx mEntity w = let
  mInv       = inventory mEntity
  mMush      = Map.findWithDefault 0 "Mushroom" mInv
  newMonster = if mMush > 0
    then let
    heal = eHP mEntity + 5
    in mEntity { inventory = Map.insert "Mushroom" (mMush-1) mInv
               , eHP = if heal > eMaxHP mEntity then eMaxHP mEntity else heal }
    else mEntity
  entry = if mMush > 0
    then T.pack "Something is Hungry..."
    else T.pack "..."
  in w { entityT = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterHeal
-- M heals slowly...
monsterHeal :: Int -> EntityKind -> World -> World
monsterHeal mx mEntity w = let
  heal = eHP mEntity + 1
  mHP = if heal > eMaxHP mEntity then eMaxHP mEntity else heal
  in w { entityT = GE.updateEntityHp mx mHP (entityT w) }

-- | monsterThrow
-- M shoots...
monsterThrow :: Int -> EntityKind -> World -> World
monsterThrow mx mEntity w = let
  mArrow = Map.findWithDefault 0 "Arrow" mInv
  mInv   = inventory mEntity
  mPos   = coord mEntity
  mTarget = mPos `elem` fovT w
  entry = if mArrow > 0 && mTarget
    then T.pack "Monster shoots..."
    else T.pack "..."
  -- throwWorld
  throwWorld = w { journalT = GJ.updateJournal [entry] (journalT w) }
  -- FoV check
  world = if mArrow > 0 && mTarget
    then GC.mkRangeCombat mx 0 throwWorld
    else w
  in world

-- | pathFinder
-- 0. Don't move the Player at 0
-- 1. Distance from goal
--   a. 5 or less is *critical*
-- 2. Decide by distance
--    a. 4 is Vision
--    b. 7 is Hear
-- 3. Check blockList
-- 4. Update move
pathFinder :: Int -> EntityKind -> World -> World
pathFinder mx mEntity w = if mx == 0 || not (block mEntity)
  then w
  else let
  coordF :: [(Int, Int)] -> [(Int, Int)]
  coordF = filter (`notElem` blockT)
  blockT = [ xy | (_, xy) <- GE.fromBlock (entityT w) ]
  (_, pPos) = GP.getPlayer (entityT w)
  -- flee goal if *critical* eHP
  mGoal  = if eHP mEntity <= 5 then mSpawn else pPos
  mPos   = coord mEntity
  mProp  = property mEntity
  mSpawn = read $ Map.findWithDefault (show pPos) "spawn" mProp :: (Int, Int)
  -- M move based on goal
  distList = [ (d, xy) | xy <- moveT mEntity, let d = EAC.chessDist mGoal xy ]
  moveList = coordF $
    [ xy | (d, pos) <- sort distList,
      let xy = if EAC.adjacent mPos mGoal || d >= 7 then mPos else pos ]
  move = if null moveList then mPos else head moveList
  in w { entityT = GE.updateEntityPos mx move (entityT w) }
