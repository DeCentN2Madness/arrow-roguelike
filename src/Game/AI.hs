{-# LANGUAGE OverloadedStrings #-}
{-

Game.AI.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.AI (aiAction) where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Engine.Arrow.Data (World(..))
import qualified Engine.Arrow.Compass as EAC
import qualified Game.Combat as GC
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..), Entity(..))
import qualified Game.Player as GP

data AI
  = Attack
  | Get
  | Eat
  | Drink
  | Move
  | Rest
  | Throw
  | Wait
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
  mItems = GE.getEntityBy mPos (entityT w)
  mPos   = coord mEntity
  mProp  = property mEntity
  mInt   = read $ Map.findWithDefault "1" "int" mProp :: Int
  mSpawn = read $ Map.findWithDefault (show mPos) "spawn" mProp :: (Int, Int)
  -- action
  action
    | EAC.adjacent pPos mPos = Attack
    | (mHp <= 5) && EAC.adjacent mSpawn mPos = Rest
    | (mArrow > 0) && (EAC.chessDist mPos pPos <= 4) = Throw
    | (mMush > 0) && (mMaxHp `div` mHp > 2) = Eat
    | (mPot > 0) && (mMaxHp `div` mHp > 3) = Drink
    | (mInt > 6) && any (\(i, _) -> kind i == Coin) mItems = Get
    | (mHp > 0) = Move
    | otherwise = Wait
  world = case action of
    Attack -> GC.mkCombat  mx 0 w
    Eat    -> monsterEat   mx mEntity w
    Get    -> monsterGet   mx mEntity w
    Drink  -> monsterDrink mx mEntity w
    Move   -> pathFinder   mx mEntity w
    Rest   -> monsterHeal  mx mEntity w
    Throw  -> monsterThrow mx mEntity w
    Wait   -> w
  -- newWorld w/ action
  in aiAction xs world

-- | monsterDrink
-- M drinks...
monsterDrink :: Int -> EntityKind -> World -> World
monsterDrink mx mEntity w = let
  mInv   = inventory mEntity
  mPot   = Map.findWithDefault 0 "Potion" mInv
  heal   = eHP mEntity + mCon
  mHp    = if heal > mMaxHp then mMaxHp else heal
  mMaxHp = eMaxHP mEntity
  mProp  = property mEntity
  mCon   = read $ Map.findWithDefault "1" "con" mProp :: Int
  newMonster = if mPot > 0
    then mEntity { inventory = Map.insert "Potion" (mPot-1) mInv, eHP = mHp }
    else mEntity
  entry = if mPot > 0
    then T.pack "Monster is Thirsty..."
    else T.pack "..."
  in w { entityT  = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterEat
-- M eats...
monsterEat :: Int -> EntityKind -> World -> World
monsterEat mx mEntity w = let
  heal   = eHP mEntity + 5
  mInv   = inventory mEntity
  mMush  = Map.findWithDefault 0 "Mushroom" mInv
  mHp    = if heal > mMaxHp then mMaxHp else heal
  mMaxHp = eMaxHP mEntity
  newMonster = if mMush > 0
    then mEntity { inventory = Map.insert "Mushroom" (mMush-1) mInv, eHP = mHp }
    else mEntity
  entry = if mMush > 0
    then T.pack "Monster is Hungry..."
    else T.pack "..."
  in w { entityT  = GE.updateEntity mx newMonster (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterGet
-- M loves Coin...
monsterGet :: Int -> EntityKind -> World -> World
monsterGet mx mEntity w = let
  mPos  = coord mEntity
  items = GE.getEntityBy mPos (entityT w)
  newMonster = if not (null items)
    then GI.pickUp items mEntity
    else mEntity
  newEntity = if not (null items)
    then GI.emptyBy mPos items (entityT w)
    else entityT w
  entry = if length items > 1
    then T.append "Monster Get " (monsterLook $ tail items)
    else T.pack "..."
  in w { entityT  = GE.updateEntity mx newMonster newEntity
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterHeal
-- M heals slowly...
monsterHeal :: Int -> EntityKind -> World -> World
monsterHeal mx mEntity w = let
  heal   = eHP mEntity + 1
  mHp    = if heal > mMaxHp then mMaxHp else heal
  mMaxHp = eMaxHP mEntity
  entry = if mHp <= 5
    then T.pack "Monster is *Hurting*..."
    else T.pack "..."
  in w { entityT  = GE.updateEntityHp mx mHp (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | monsterLook
-- if there is something to see...
monsterLook :: [(EntityKind, a)] -> Text
monsterLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if not (block ek)
                          then T.pack $ show (kind ek) ++ ", "
                          else "" ]
  in T.append look "..."

-- | monsterThrow
-- M shoots...
monsterThrow :: Int -> EntityKind -> World -> World
monsterThrow mx mEntity w = let
  mInv    = inventory mEntity
  mArrow  = Map.findWithDefault 0 "Arrow" mInv
  mPos    = coord mEntity
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
