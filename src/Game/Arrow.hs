{-# LANGUAGE OverloadedStrings #-}
{-

Game.Arrow.hs

Game.Arrow takes Intent and `applyIntent` to the World. This is the
main control for the Player '@' in the Game.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Arrow (applyIntent) where

import Prelude hiding (lookup)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Data
import qualified Engine.Arrow.View as EAV
import qualified Engine.Draw.Camera as EDC
import qualified Game.AI as GAI
import qualified Game.Combat as GC
import Game.Compass
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import qualified Game.Inventory as GI
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP
import qualified Game.Tile as GT

-- | actionBump
-- if there is a bump...
actionBump :: Coord -> EntityMap -> Int
actionBump pos em = let
  blockList = filter ((==pos).snd) $ GE.fromBlock em
  in if null blockList
    then 0
    else fst $ head blockList

-- | actionCast
-- if there is something to cast...
-- TODO animate magic
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
    then pEntity { eMP = if pMana - 1 > 0 then 0 else pMana - 1}
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
-- if there is $ to spend...
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
  newInv = if pCoin > 0 && snd pItem < 20
    then Map.insert "Coin" (pCoin-1) pInv
    else pInv
  newPlayer = if fst pItem /= "None" && snd pItem < 20
    then let
    in pEntity { inventory = Map.insert (fst pItem) (snd pItem+1) newInv }
    else pEntity
  entry = if fst pItem /= "None" && snd pItem < 20
    then T.concat [fst pItem, " +1, ..."]
    else "No Spend... "
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionDirection the world will change with @input@
-- 1. clampCoord checks the input vs grid
-- 2. actionPlayer handles P events in the World
-- 3. actionMonster handles M events in the World
-- 4. update Camera
actionDirection :: Direction -> World -> World
actionDirection input w = if gameState w == GameStart
  then EDC.updateCamera w { gameState = GameRun }
  else let
    -- oldWorld
    (_, playerCoord) = GP.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    -- newWorld from Monster && Player Events
    world            = actionMonster $ actionPlayer clampCoord w
    (_, pPos)        = GP.getPlayer (entityT world)
    run = if pPos == clampCoord
      then EAV.updateView $ world { fovT = EAV.mkView pPos (gameT world)
                                  , dirty = True }
      else world { dirty = False }
  in EDC.updateCamera run

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
    in pEntity { inventory = Map.insert (snd pItem) 1 (inventory pEntity)
               , property  = Map.insert pEquip "None" (property pEntity) }
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
    in pEntity { inventory = Map.insert (fst pItem) (snd pItem-1) newInv
               , property  = Map.insert pEquip (fst pItem) (property pEntity) }
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
-- if there is something to drop...
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
-- if there is something to eat...
actionEat :: World -> World
actionEat w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  -- Mushroom
  pInv   = inventory pEntity
  pMush  = Map.findWithDefault 0 "Mushroom" pInv
  heal   = eHP pEntity + (pCon `div` 2)
  pHp    = if heal > pMaxHp then pMaxHp else heal
  pMaxHp = eMaxHP pEntity
  pProp  = property pEntity
  pCon   = read $ T.unpack $ Map.findWithDefault "1" "con" pProp
  newPlayer = if pMush > 0
    then pEntity { inventory = Map.insert "Mushroom" (pMush-1) pInv, eHP = pHp }
    else pEntity
  entry = if pMush > 0
    then T.pack "Eat a tasty :Mushroom:..."
    else "No Eat..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }


-- | actionGet
-- if there is something to get...
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
-- if there is something to hear...
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
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  groupF :: [Text] -> [(Text, Int)]
  groupF = map (head &&& length) . group . sort
  -- items
  items = groupF $ filter (/="Player") $
    [ name | (ek, _) <- xs,
      let name = snd $ T.breakOnEnd "/" $
            Map.findWithDefault "I" "Name" (property ek) ]
  look = T.concat $ [ e | (i, j) <- items,
                      let e = if j > 1
                            then T.append i $ T.pack $ " <" ++ show j ++ ">, "
                            else T.append i $ T.pack ", " ]
  in T.append look "..."

-- | actionMonster
-- 1. Where can P and M move in relation to Terrain
-- 2. aiAction based on newWorld
-- 3. pathFinder based on aiAction
actionMonster :: World -> World
actionMonster w = let
  entityList = [ (ix, ek) | (e, ix) <- GE.fromEntityAt (entityT w),
                 let ek = if block e -- Movable Entity
                       then let
                       move = cardinal (coord e)
                       in e { moveT = move `GT.fromMoveBlocked` gameT w }
                       else e ]
  in GAI.aiAction entityList w { entityT = Map.fromList entityList }

-- | actionPlayer
-- handle the Player within the World
actionPlayer :: Coord -> World -> World
actionPlayer pos w = let
  -- Time event
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  -- Bump event
  bump      = actionBump pos (entityT w)
  bumpCoord = if bump < 1 then pos else pPos
  -- Move event
  newCoord  = if bumpCoord `elem` moveT pEntity then bumpCoord else pPos
  moveWorld = w { entityT = GP.updatePlayerBy newCoord (entityT w) }
  -- Look && Listen event
  look      = actionLook $ GE.getEntityBy newCoord (entityT moveWorld)
  listen    = actionHear (entityT moveWorld) newCoord
  -- Combat event
  world     = if bump > 0 then GC.mkCombat 0 bump moveWorld else moveWorld
  -- XP event
  (newPlayer, _) = GP.getPlayer (entityT world)
  learn = if eLvl newPlayer > eLvl pEntity
    then T.pack $ "Welcome to Level " ++ show (eLvl newPlayer) ++ "..."
    else "..."
  alive = if eHP newPlayer < 1
    then "Player is Dead! Press r to Restart..."
    else "..."
  entry = [look, listen, learn, alive]
  in world { tick     = newTick
           , journalT = GJ.updateJournal entry (journalT world) }

-- | actionQuaff
-- if there is something to drink...
actionQuaff :: World -> World
actionQuaff w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  -- Potion
  pInv   = inventory pEntity
  pPot   = Map.findWithDefault 0 "Potion" pInv
  heal   = eHP pEntity + pCon
  mana   = eMP pEntity + pWis
  pHp    = if heal > pMaxHp then pMaxHp else heal
  pMp    = if mana > pMaxMp then pMaxMp else mana
  pMaxHp = eMaxHP pEntity
  pMaxMp = eMaxMP pEntity
  pProp  = property pEntity
  pCon   = read $ T.unpack $ Map.findWithDefault "1" "con" pProp
  pWis   = read $ T.unpack $ Map.findWithDefault "1" "wis" pProp
  newPlayer = if pPot > 0
    then pEntity { inventory = Map.insert "Potion" (pPot-1) pInv
                 , eHP = pHp
                 , eMP = pMp }
    else pEntity
  entry = if pPot > 0
    then T.pack "Drink a delicious :Potion:..."
    else "No Drink..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionThrow
-- if there is something to throw...
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

-- | applyIntent
-- Events applied to the World
-- Currently, Player Turn then Monster Turn...
-- TODO build equip k/v
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  world n
    | n == GameDrop = case intent of
        Action Zero  -> actionDrop 0 w
        Action One   -> actionDrop 1 w
        Action Two   -> actionDrop 2 w
        Action Three -> actionDrop 3 w
        Action Four  -> actionDrop 4 w
        Action Five  -> actionDrop 5 w
        Action Six   -> actionDrop 6 w
        Action Seven -> actionDrop 7 w
        Action Eight -> actionDrop 8 w
        Action Nine  -> actionDrop 9 w
        Action A  -> actionDrop 10 w
        Action B  -> actionDrop 11 w
        Action C  -> actionDrop 12 w
        Action D  -> actionDrop 13 w
        Action E  -> actionDrop 14 w
        Action F  -> actionDrop 15 w
        Action G  -> actionDrop 16 w
        Action H  -> actionDrop 17 w
        Action I  -> actionDrop 18 w
        Action J  -> actionDrop 19 w
        Quit -> escWorld w
        _ -> w
    | n == GameEquipment = case intent of
        Action Zero  -> actionDoff "melee" w
        Action One   -> actionDoff "shoot" w
        Action Two   -> actionDoff "jewelry" w
        Action Three -> actionDoff "neck" w
        Action Four  -> actionDoff "armor" w
        Action Five  -> actionDoff "cloak" w
        Action Six   -> actionDoff "shield" w
        Action Seven -> actionDoff "head" w
        Action Eight -> actionDoff "hands" w
        Action Nine  -> actionDoff "feet" w
        Quit -> escWorld w
        _ -> w
    | n == GameInventory = case intent of
        Action Zero  -> actionDon 0 w
        Action One   -> actionDon 1 w
        Action Two   -> actionDon 2 w
        Action Three -> actionDon 3 w
        Action Four  -> actionDon 4 w
        Action Five  -> actionDon 5 w
        Action Six   -> actionDon 6 w
        Action Seven -> actionDon 7 w
        Action Eight -> actionDon 8 w
        Action Nine  -> actionDon 9 w
        Action A  -> actionDon 10 w
        Action B  -> actionDon 11 w
        Action C  -> actionDon 12 w
        Action D  -> actionDon 13 w
        Action E  -> actionDon 14 w
        Action F  -> actionDon 15 w
        Action G  -> actionDon 16 w
        Action H  -> actionDon 17 w
        Action I  -> actionDon 18 w
        Action J  -> actionDon 19 w
        Quit -> escWorld w
        _ -> w
    | n == GameStore = case intent of
        Action Zero  -> actionCoin 0 w
        Action One   -> actionCoin 1 w
        Action Two   -> actionCoin 2 w
        Action Three -> actionCoin 3 w
        Action Four  -> actionCoin 4 w
        Action Five  -> actionCoin 5 w
        Action Six   -> actionCoin 6 w
        Action Seven -> actionCoin 7 w
        Action Eight -> actionCoin 8 w
        Action Nine  -> actionCoin 9 w
        Quit -> escWorld w
        _ -> w
    | otherwise = case intent of
        Action North     -> actionDirection North w
        Action NorthEast -> actionDirection NorthEast w
        Action East      -> actionDirection East w
        Action SouthEast -> actionDirection SouthEast w
        Action South     -> actionDirection South w
        Action SouthWest -> actionDirection SouthWest w
        Action West      -> actionDirection West w
        Action NorthWest -> actionDirection NorthWest w
        Action A -> coinWorld w
        Action B -> actionDirection SouthWest w
        Action C -> actionMonster $ actionCast w
        Action D -> dropWorld w
        Action E -> actionMonster $ actionEat w
        Action G -> actionGet w
        Action H -> actionDirection East w
        Action I -> invWorld w
        Action J -> actionDirection South w
        Action K -> actionDirection North w
        Action L -> actionDirection West w
        Action N -> actionDirection SouthEast w
        Action Q -> actionMonster $ actionQuaff w
        Action R -> resetWorld w
        Action T -> actionMonster $ actionThrow w
        Action U -> actionDirection NorthWest w
        Action W -> equipWorld w
        Action Y -> actionDirection NorthEast w
        Quit -> escWorld w
        _ -> w
  in world (gameState w)

-- | coinWorld
-- store mode
coinWorld :: World -> World
coinWorld w = w { journalT = GJ.updateJournal ["A Pressed..."] (journalT w)
                 , gameState = if gameState w == GameStore
                   then GameRun
                   else GameStore }

-- | dropWorld
-- Equipment mode
dropWorld :: World -> World
dropWorld w = w { journalT = GJ.updateJournal ["D Pressed..."] (journalT w)
                 , gameState = if gameState w == GameDrop
                   then GameRun
                   else GameDrop }

-- | escWorld
-- ESC changes gameState
escWorld :: World -> World
escWorld w = w { journalT = GJ.updateJournal ["ESC Pressed..."] (journalT w)
               , gameState = if gameState w == GameRun
                 then GameStop
                 else GameRun }

-- | equipWorld
-- Equipment mode
equipWorld :: World -> World
equipWorld w = w { journalT = GJ.updateJournal ["W Pressed..."] (journalT w)
                 , gameState = if gameState w == GameEquipment
                   then GameRun
                   else GameEquipment }

-- | invWorld
-- Inventory mode
invWorld :: World -> World
invWorld w = w { journalT = GJ.updateJournal ["I Pressed..."] (journalT w)
                    , gameState = if gameState w == GameInventory
                      then GameRun
                      else GameInventory }

-- | resetWorld, save the Player, and rebuild the World
-- TODO depth set by stairs...
resetWorld :: World -> World
resetWorld w = let
  (maxX, maxY) = screenXY w
  (row, col)   = gridXY w
  (p, _)       = GP.getPlayer (entityT w)
  world        = mkWorld (tick w) (floor maxX, floor maxY) (eLvl p) row col
  entry        = T.pack $ "Depth " ++ show (50 * eLvl p) ++ "'..."
  in world { entityT  = GE.safeInsertEntity 0 p (gameT world) (entityT world)
           , journalT = GJ.updateJournal ["R pressed...", entry] (journalT w) }
