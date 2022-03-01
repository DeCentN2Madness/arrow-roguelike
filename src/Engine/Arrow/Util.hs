{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Compass
import Engine.Arrow.Data
import qualified Engine.Arrow.View as EAV
import qualified Engine.Draw.Camera as EDC
import qualified Game.AI as GAI
import qualified Game.Combat as GC
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

-- | actionDirection the world will change with @input@
-- 1. clampCoord checks the input vs grid
-- 2. actionPlayer handles P events in the World
-- 3. actionMonster handles M events in the World
-- 4. update Camera
actionDirection :: Direction -> World -> World
actionDirection input w = if starting w
  then EDC.updateCamera w { starting = False }
  else let
    -- oldWorld
    (_, playerCoord) = GP.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    -- newWorld from Monster && Player Events
    world     = actionMonster $ actionPlayer clampCoord w
    (_, pPos) = GP.getPlayer (entityT world)
    run = if pPos == clampCoord
      then EAV.updateView $ world { fovT = EAV.mkView pPos (gameT world)
                                  , dirty = True }
      else world { dirty = False }
  in EDC.updateCamera run

-- | actionEat
-- if there is something to eat...
actionEat :: World -> World
actionEat w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pInv         = inventory pEntity
  pMush        = Map.findWithDefault 0 "Mushroom" pInv
  newPlayer = if pMush > 0
    then let
    heal = eHP pEntity + 5
    in pEntity { inventory = Map.insert "Mushroom" (pMush-1) pInv
               , eHP = if heal > eMaxHP pEntity then eMaxHP pEntity else heal }
    else pEntity
  entry = if pMush > 0
    then T.pack "Eat a tasty Mushroom..."
    else T.pack "Nothing to eat..."
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
  newPlayer = if not (null items)
    then GI.pickup items pEntity
    else pEntity
  newEntity = if not (null items)
    then GI.emptyBy pPos items (entityT w)
    else entityT w
  entry = if length items > 1
    then T.append "Get " (actionLook $ tail items)
    else T.pack "Nothing interesting..."
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
  hear x
    | x > 1  = T.pack $ "Something moved " ++ "<x" ++ show x ++ ">, "
    | x == 1 = T.pack "Something moved, "
    | otherwise = ""
  in T.append (hear total) "..."

-- | actionLook
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if not (block ek)
                          then T.pack $ show (kind ek) ++ ", "
                          else "" ]
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
  -- newWorld w/ fromMoveBlocked
  world = w { entityT = Map.fromList entityList }
  in GAI.pathFinder entityList $ GAI.aiAction entityList world

-- | actionPlayer
-- handle the Player within the World
actionPlayer :: Coord -> World -> World
actionPlayer pos w = let
  -- Time event
  newTick         = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  -- Bump event
  bump       = actionBump pos (entityT w)
  bumpCoord  = if bump < 1 then pos else pPos
  -- Move event
  newCoord   = if bumpCoord `elem` moveT pEntity then bumpCoord else pPos
  moveWorld  = w { entityT = GP.updatePlayerBy newCoord (entityT w) }
  -- Look && Listen event
  look       = actionLook $ GE.getEntityBy newCoord (entityT moveWorld)
  listen     = actionHear (entityT moveWorld) newCoord
  -- Combat event
  world  = if bump > 0 then GC.mkCombat 0 bump moveWorld else moveWorld
  -- XP event
  (p, _) = GP.getPlayer (entityT world)
  learn = if eLvl p > eLvl pEntity
    then T.pack $ "Welcome to Level " ++ show (eLvl p) ++ "..."
    else "..."
  in world { tick = newTick
           , journalT = GJ.updateJournal [look, listen, learn] (journalT world) }

-- | actionQuaff
-- if there is something to drink...
actionQuaff :: World -> World
actionQuaff w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pInv         = inventory pEntity
  pPot         = Map.findWithDefault 0 "Potion" pInv
  newPlayer = if pPot > 0
    then let
    heal = eHP pEntity + 7
    in pEntity { inventory = Map.insert "Potion" (pPot-1) pInv
               , eHP = if heal > eMaxHP pEntity then eMaxHP pEntity else heal }
    else pEntity
  entry = if pPot > 0
    then T.pack "Drink a delicious Potion..."
    else T.pack "Nothing to drink..."
  in w { tick = newTick
       , entityT  = GP.updatePlayer newPlayer (entityT w)
       , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionThrow
-- if there is something to throw...
-- TODO animate throw
actionThrow :: World -> World
actionThrow w = let
  newTick      = tick w + 1
  (pEntity, _) = GP.getPlayer (entityT w)
  pInv         = inventory pEntity
  pUnk         = Map.findWithDefault 0 "Unknown" pInv
  -- pick closest target
  mTargets = filter (\(_, j) -> j `elem` fovT w) $
    [ (ix, xy) | (ix, pos) <- GE.fromBlock (entityT w),
      let xy = if ix > 0 then pos else (0,0) ]
  mTarget = case mTargets of
    [] -> 0
    ((ix, _):_) -> ix
  newPlayer = if pUnk > 0 && mTarget > 0
    then pEntity { inventory = Map.insert "Unknown" (pUnk-1) pInv }
    else pEntity
  entry = if pUnk > 0 && mTarget > 0
    then T.pack "Shoots an Arrow..."
    else T.pack "No Arrows..."
  -- throwWorld
  throwWorld = w { entityT  = GP.updatePlayer newPlayer (entityT w)
                 , journalT = GJ.updateJournal [entry] (journalT w) }
  -- Combat event
  world = if pUnk > 0 && mTarget > 0
    then GC.mkRangeCombat 0 mTarget throwWorld
    else throwWorld
  in world { tick = newTick }

-- | applyIntent
-- Events applied to the World
-- Currently, Player Turn then Monster Turn...
-- TODO action stack
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  world = case intent of
    Action North     -> actionDirection North w
    Action NorthEast -> actionDirection NorthEast w
    Action East      -> actionDirection East w
    Action SouthEast -> actionDirection SouthEast w
    Action South     -> actionDirection South w
    Action SouthWest -> actionDirection SouthWest w
    Action West      -> actionDirection West w
    Action NorthWest -> actionDirection NorthWest w
    Action C -> showCharacter w
    Action E -> actionMonster $ actionEat w
    Action G -> actionMonster $ actionGet w
    Action I -> showInventory w
    Action Q -> actionMonster $ actionQuaff w
    Action R -> resetWorld w
    Action T -> actionMonster $ actionThrow w
    Quit     -> quitWorld w
    _ -> w
  in world

-- | resetWorld, save the Player, and rebuild the World
-- TODO depth set by stairs and player level
resetWorld :: World -> World
resetWorld w = let
  (maxX, maxY) = screenXY w
  (row, col)   = gridXY w
  (p, _)       = GP.getPlayer (entityT w)
  world        = mkWorld (tick w) (floor maxX, floor maxY) (eLvl p) row col
  in world { entityT = GE.safeInsertEntity 0 p (gameT world) (entityT world) }

-- | showCharacter
showCharacter :: World -> World
showCharacter w = let
  entry = T.intercalate ", " $ filter (/=" ") $ GP.characterSheet (entityT w)
  in w { journalT = GJ.updateJournal [entry] (journalT w) }

-- | showInventory
showInventory :: World -> World
showInventory w = let
  entry = T.intercalate ", " $ GP.characterInventory (entityT w)
  in w { journalT = GJ.updateJournal [entry] (journalT w) }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { journalT = GJ.updateJournal ["Saving Game..."] (journalT w)
                , exiting = True }
