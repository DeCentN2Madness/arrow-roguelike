{-# LANGUAGE OverloadedStrings #-}
{-

Game.Action.hs

Game.Action.hs is the actions for the Player '@' in the Game.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Action where

import Prelude hiding (lookup)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Data
import qualified Game.Combat as GC
import Game.Compass
import qualified Game.DiceSet as DS
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | actionCast
-- if there is something to Cast...
actionCast :: World -> World
actionCast w = let
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  pMana           = eMP pEntity
  mySort          = sortBy (compare `on` snd)
  -- pick closest target
  mTargets = filter (\(_, j) -> j `elem` fovT w) $
    [ (ix, xy) | (ix, pos) <- GE.fromBlock (entityT w),
      let xy = if ix > 0 then pos else (0,0) ]
  mTarget = case mTargets of
    [] -> 0
    xs -> fst $ head $ mySort [ (ix, d) | (ix, xy) <- xs,
                                let d = distance xy pPos ]
  newPlayer = if pMana > 0 && mTarget > 0
    then pEntity { eMP = if pMana - 1 > 0 then pMana - 1 else 0 }
    else pEntity
  entry = if pMana > 0 && mTarget > 0
    then T.pack "Casts a Spell..."
    else T.pack "No Cast..."
  -- throwWorld
  throwWorld = w { entityT  = GP.updatePlayer newPlayer (entityT w)
                 , journalT = GJ.updateJournal [entry] (journalT w) }
  -- Combat event
  world = if pMana > 0 && mTarget > 0
    then GC.mkMagicCombat 0 mTarget throwWorld
    else throwWorld
  in world { tick = newTick }

-- | actionCoin
-- if there is $ to Spend...
actionCoin :: Int -> World -> World
actionCoin ix w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pInv = inventory pEntity
  pCoin = Map.findWithDefault 0 "Coin" pInv
  pItems = filter (\(i, _) -> i `elem` ["Arrow", "Mushroom", "Potion"]) $
           Map.toList pInv
  pItem = if ix < length pItems
    then pItems!!ix
    else ("None", 0)
  newInv = if pCoin >= 1 && snd pItem < 20
    then Map.insert "Coin" (pCoin-1) pInv
    else pInv
  newPlayer = if pCoin >= 1 && fst pItem /= "None" && snd pItem < 20
    then let
    in pEntity { inventory = Map.insert (fst pItem) (snd pItem+1) newInv }
    else pEntity
  entry = if pCoin >= 1 && fst pItem /= "None" && snd pItem < 20
    then T.concat [fst pItem, " +1, ..."]
    else "No Spend... "
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionDoff
-- if there is a Hat to remove...
actionDoff :: Text -> World -> World
actionDoff item w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pEquip = fst $ T.breakOn "/" item
  pItems = filter ((==item).fst) $ Map.toList (property pEntity)
  pItem = if not (null pItems)
    then head pItems
    else ("None", "None")
  newPlayer = if snd pItem /= "None"
    then let
    newProp = Map.insert pEquip "None" (property pEntity)
    in pEntity { inventory = Map.insert (snd pItem) 1 (inventory pEntity)
               , property  = GP.armorShield newProp (assetT w) }
    else pEntity
  entry = if not (null pItems)
    then T.concat ["Doff ", fst pItem, "..."]
    else "No Doff..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionDon
-- if there is Hat to Wear...
actionDon :: Int -> World -> World
actionDon ix w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pInv = inventory pEntity
  pItems = filter (\(i, j) -> j > 0 &&
                     i `notElem` ["Arrow", "Coin", "Mushroom", "Potion"]) $
           Map.toList pInv
  pItem = if ix < length pItems
    then pItems!!ix
    else ("None", 0)
  pEquip = fst $ T.breakOn "/" (fst pItem)
  -- add old equip into inventory
  equip =  Map.findWithDefault "None" pEquip (property pEntity)
  newInv = if equip /= "None"
    then Map.insert equip 1 pInv
    else pInv
  newPlayer = if snd pItem > 0
    then let
    newProp = Map.insert pEquip (fst pItem) (property pEntity)
    in pEntity { inventory = Map.insert (fst pItem) (snd pItem-1) newInv
               , property  = GP.armorShield newProp (assetT w) }
    else pEntity
  entry = if fst pItem /= "None"
    then if equip /= "None"
    then T.concat ["Doff ", equip, ", Don ", fst pItem, "..."]
    else T.concat ["Don ", fst pItem, "..."]
    else "No Don..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionDrop
-- if there is something to Drop...
actionDrop :: Int -> World -> World
actionDrop ix w = let
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  pInv = inventory pEntity
  pItems = filter (\(i, j) -> j > 0 &&
                     i `notElem` ["Arrow", "Coin", "Mushroom", "Potion"]) $
           Map.toList pInv
  pItem = if ix < length pItems
    then pItems!!ix
    else ("None", 0)
  count = Map.findWithDefault 0 (fst pItem) pInv
  item = GI.mkDropItem (fst pItem) pPos (assetT w)
  newPlayer = if count > 0
    then pEntity { inventory = Map.insert (fst pItem) (count-1) pInv }
    else pEntity
  newEntity = if count > 0
    then GI.putDown item (entityT w)
    else entityT w
  entry = if count > 0
    then T.append "Drop " (actionLook [(item, pPos)])
    else "No Drop..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer newEntity
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionEat
-- if there is something to Eat...
actionEat :: World -> World
actionEat w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  -- Mushroom
  pInv   = inventory pEntity
  pMush  = Map.findWithDefault 0 "Mushroom" pInv
  hRoll  = DS.d4 newTick + pCon
  heal   = eHP pEntity + hRoll
  pHp    = if heal > pMaxHp then pMaxHp else heal
  pMaxHp = eMaxHP pEntity
  pProp  = property pEntity
  pCon   = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "con" pProp
  newPlayer = if pMush > 0
    then pEntity { inventory = Map.insert "Mushroom" (pMush-1) pInv, eHP = pHp }
    else pEntity
  entry = if pMush > 0
    then T.append "Eat a tasty :Mushroom:" $
    T.pack $ " <+" ++ show hRoll ++ ">, ..."
    else "No Eat..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionExamine
-- if there is something to Examine...
actionExamine :: Int -> World -> World
actionExamine x w = let
  -- description
  descMap = Map.fromList $
    [ (name, desc) | (_, v) <- Map.toList (assetT w),
      let name = Map.findWithDefault "None" "Name" (property v)
          desc = Map.findWithDefault "None" "Description" (property v) ]
  -- properties
  propMap = Map.fromList $
    [ (name, prop) | (_, v) <- Map.toList (entityT w),
      let name = Map.findWithDefault "None" "Name" (property v)
          prop = property v ]
  view = GE.fromEntityBy (entityT w)
  pFOV = [ name | (ek, _) <- filter (\(_, j) -> j `elem` fovT w) view,
           let name = Map.findWithDefault "None" "Name" (property ek) ]
  sz   = length pFOV - 1
  mSel = if x > sz then 0 else x
  mName = pFOV!!mSel
  mExamine = Map.findWithDefault "None" mName descMap
  mProp    = Map.findWithDefault Map.empty mName propMap
  mStr     = Map.findWithDefault "0" "str" mProp
  mDex     = Map.findWithDefault "0" "dex" mProp
  mCon     = Map.findWithDefault "0" "con" mProp
  mInt     = Map.findWithDefault "0" "int" mProp
  mWis     = Map.findWithDefault "0" "wis" mProp
  mClass   = Map.findWithDefault "Item"          "Class"  mProp
  mArmor   = Map.findWithDefault "armor/Natural" "armor"  mProp
  mAC      = Map.findWithDefault "0"             "AC"     mProp
  mMelee   = Map.findWithDefault "melee/Natural" "melee"  mProp
  mRange   = Map.findWithDefault "None"          "shoot"  mProp
  mAttack  = Map.findWithDefault "1d4"           "ATTACK" mProp
  mShoot   = Map.findWithDefault "0"             "SHOOT"  mProp
  mCls     = T.concat [ "Class: ", mClass ]
  mStat    = if mStr /= "0"
    then T.concat [ "Str:",   mStr
                  , ", Dex:", mDex
                  , ", Con:", mCon
                  , ", Int:", mInt
                  , ", Wis:", mWis ]
    else "..."
  mEquip   = if mAC /= "0"
    then T.concat [ "AC:",  mArmor, " (", mAC, ")"
                  , ", M:", mMelee, " (", mAttack, ")"
                  , ", R:", mRange, " (", mShoot, ")" ]
    else "..."
  entry = if mExamine /= "None"
    then T.concat [ mName,  ": ", mExamine ]
    else "No Examine..."
  in w { journalT = GJ.updateJournal [mEquip, mStat, mCls, entry] (journalT w) }

-- | actionGet
-- if there is something to Get...
actionGet :: World -> World
actionGet w = let
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  items           = GE.getEntityBy pPos (entityT w)
  pickedItems     = GI.checkPickUp (inventory newPlayer) (inventory pEntity)
  newPlayer = if not (null items)
    then GI.pickUp items pEntity
    else pEntity
  newEntity = if pickedItems /= Map.empty
    then GI.emptyBy pPos items (entityT w)
    else entityT w
  entry = if pickedItems /= Map.empty
    then T.append "Get " (actionLook items)
    else "No Get..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer newEntity
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionHear
-- if there is something to Hear...
actionHear :: EntityMap -> Coord -> Text
actionHear em listen = let
  hearList = [ t | (ek, _) <- GE.fromEntityAt em,
               let t = if block ek && (d >= 4 && d <= 7) then 1 else 0
                   d = distance (coord ek) listen ]
  total = sum hearList :: Int
  hear n
    | n > 1  = T.pack $ "Something moved " ++ " <" ++ show n ++ ">, "
    | n == 1 = T.pack "Something moved, "
    | otherwise = ""
  in T.append (hear total) "..."

-- | actionLook
-- if there is something to See...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  groupF :: [Text] -> [(Text, Int)]
  groupF = map (head &&& length) . group . sort
  -- items
  items = groupF $ filter (/="Player") $
    [ name | (ek, _) <- xs,
      let name = snd $ T.breakOnEnd "/" $
            Map.findWithDefault "I" "Name" (property ek) ]
  look = T.take 60 $ T.concat $
    [ e | (i, j) <- items,
      let e = if j > 1
            then T.append i $ T.pack $ " <" ++ show j ++ ">, "
            else T.append i $ T.pack ", " ]
  in T.append look "..."

-- | actionQuaff
-- if there is something to Drink...
actionQuaff :: World -> World
actionQuaff w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  -- Potion
  seed   = tick w
  pInv   = inventory pEntity
  pPot   = Map.findWithDefault 0 "Potion" pInv
  hRoll  = DS.d4 seed     + DS.d4 (seed+1) + pCon
  mRoll  = DS.d4 (seed+2) + DS.d4 (seed+3) + pWis
  heal   = eHP pEntity + hRoll
  mana   = eMP pEntity + mRoll
  pHp    = if heal > pMaxHp then pMaxHp else heal
  pMp    = if mana > pMaxMp then pMaxMp else mana
  pMaxHp = eMaxHP pEntity
  pMaxMp = eMaxMP pEntity
  pProp  = property pEntity
  pCon   = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "con" pProp
  pWis   = abilityMod $ read $ T.unpack $ Map.findWithDefault "1" "wis" pProp
  newPlayer = if pPot > 0
    then pEntity { inventory = Map.insert "Potion" (pPot-1) pInv
                 , eHP = pHp, eMP = pMp }
    else pEntity
  entry = if pPot > 0
    then T.append "Drink a delicious :Potion:" $
    T.pack $ " <+" ++ show hRoll ++ ", +" ++ show mRoll ++ ">, ..."
    else "No Drink..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionRest
-- if there is time to Rest...
-- TODO regeneration
actionRest :: World -> World
actionRest w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  heal   = eHP pEntity + 1
  mana   = eMP pEntity + 1
  pHp    = if heal > pMaxHp then pMaxHp else heal
  pMp    = if mana > pMaxMp then pMaxMp else mana
  pMaxHp = eMaxHP pEntity
  pMaxMp = eMaxMP pEntity
  newPlayer = pEntity { eHP = pHp, eMP = pMp }
  entry = T.append "Rest... tick=" (T.pack $ show newTick)
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionThrow
-- if there is something to Throw...
-- TODO animate throw
actionThrow :: World -> World
actionThrow w = let
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  pInv            = inventory pEntity
  pArrow          = Map.findWithDefault 0 "Arrow" pInv
  mySort          = sortBy (compare `on` snd)
  -- pick closest target
  mTargets = filter (\(_, j) -> j `elem` fovT w) $
    [ (ix, xy) | (ix, pos) <- GE.fromBlock (entityT w),
      let xy = if ix > 0 then pos else (0,0) ]
  mTarget = case mTargets of
    [] -> 0
    xs -> fst $ head $ mySort [ (ix, d) | (ix, xy) <- xs,
                                let d = distance xy pPos ]
  newPlayer = if pArrow > 0 && mTarget > 0
    then pEntity { inventory = Map.insert "Arrow" (pArrow-1) pInv }
    else pEntity
  entry = if pArrow > 0 && mTarget > 0
    then T.pack "Shoots an Arrow..."
    else "No Shoot..."
  -- throwWorld
  throwWorld = w { entityT  = GP.updatePlayer newPlayer (entityT w)
                 , journalT = GJ.updateJournal [entry] (journalT w) }
  -- Combat event
  world = if pArrow > 0 && mTarget > 0
    then GC.mkRangeCombat 0 mTarget throwWorld
    else throwWorld
  in world { tick = newTick }
