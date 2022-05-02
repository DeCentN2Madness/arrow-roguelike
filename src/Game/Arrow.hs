{-# LANGUAGE OverloadedStrings #-}
{-

Game.Arrow.hs

Game.Arrow.hs takes Intent and `applyIntent` to the World.
Game.Arrow.hs also controls GameStates for the Game.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Arrow (applyIntent) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Arrow.Data
import qualified Engine.Arrow.View as EAV
import qualified Engine.Draw.Camera as EDC
import qualified Game.Action as GA
import qualified Game.AI as GAI
import qualified Game.Combat as GC
import Game.Compass
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import qualified Game.Journal as GJ
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Player as GP
import qualified Game.Tile as GT

-- | actionBump
-- if there is a Bump...
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
  -- Listen event
  listen    = GA.actionHear (entityT moveWorld) newCoord
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
  entry = [listen, learn, alive]
  in world { tick     = newTick
           , journalT = GJ.updateJournal entry (journalT world) }

-- | applyIntent
-- Events applied to the World
-- Currently, Player Turn then Monster Turn...
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  world n
    | n == GameDrop = case intent of
        Action Zero  -> GA.actionDrop 0 w
        Action One   -> GA.actionDrop 1 w
        Action Two   -> GA.actionDrop 2 w
        Action Three -> GA.actionDrop 3 w
        Action Four  -> GA.actionDrop 4 w
        Action Five  -> GA.actionDrop 5 w
        Action Six   -> GA.actionDrop 6 w
        Action Seven -> GA.actionDrop 7 w
        Action Eight -> GA.actionDrop 8 w
        Action Nine  -> GA.actionDrop 9 w
        Action A  -> GA.actionDrop 10 w
        Action B  -> GA.actionDrop 11 w
        Action C  -> GA.actionDrop 12 w
        Action D  -> GA.actionDrop 13 w
        Action E  -> GA.actionDrop 14 w
        Action F  -> GA.actionDrop 15 w
        Action G  -> GA.actionDrop 16 w
        Action H  -> GA.actionDrop 17 w
        Action I  -> GA.actionDrop 18 w
        Action J  -> GA.actionDrop 19 w
        Action W  -> equipWorld w
        Quit -> escWorld w
        _ -> w
    | n == GameEquipment = case intent of
        Action Zero  -> GA.actionDoff "melee" w
        Action One   -> GA.actionDoff "shoot" w
        Action Two   -> GA.actionDoff "jewelry" w
        Action Three -> GA.actionDoff "neck" w
        Action Four  -> GA.actionDoff "armor" w
        Action Five  -> GA.actionDoff "cloak" w
        Action Six   -> GA.actionDoff "shield" w
        Action Seven -> GA.actionDoff "head" w
        Action Eight -> GA.actionDoff "hands" w
        Action Nine  -> GA.actionDoff "feet" w
        Action I     -> invWorld w
        Quit -> escWorld w
        _ -> w
    | n == GameInventory = case intent of
        Action Zero  -> GA.actionDon 0 w
        Action One   -> GA.actionDon 1 w
        Action Two   -> GA.actionDon 2 w
        Action Three -> GA.actionDon 3 w
        Action Four  -> GA.actionDon 4 w
        Action Five  -> GA.actionDon 5 w
        Action Six   -> GA.actionDon 6 w
        Action Seven -> GA.actionDon 7 w
        Action Eight -> GA.actionDon 8 w
        Action Nine  -> GA.actionDon 9 w
        Action A  -> GA.actionDon 10 w
        Action B  -> GA.actionDon 11 w
        Action C  -> GA.actionDon 12 w
        Action D  -> GA.actionDon 13 w
        Action E  -> GA.actionDon 14 w
        Action F  -> GA.actionDon 15 w
        Action G  -> GA.actionDon 16 w
        Action H  -> GA.actionDon 17 w
        Action I  -> GA.actionDon 18 w
        Action J  -> GA.actionDon 19 w
        Action W  -> equipWorld w
        Quit -> escWorld w
        _ -> w
    | n == GameStore = case intent of
        Action Zero  -> GA.actionCoin 0 w
        Action One   -> GA.actionCoin 1 w
        Action Two   -> GA.actionCoin 2 w
        Action Three -> GA.actionCoin 3 w
        Action Four  -> GA.actionCoin 4 w
        Action Five  -> GA.actionCoin 5 w
        Action Six   -> GA.actionCoin 6 w
        Action Seven -> GA.actionCoin 7 w
        Action Eight -> GA.actionCoin 8 w
        Action Nine  -> GA.actionCoin 9 w
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
        Action C -> actionMonster $ GA.actionCast w
        Action D -> dropWorld w
        Action E -> actionMonster $ GA.actionEat w
        Action G -> actionMonster $ GA.actionGet w
        Action H -> actionDirection East w
        Action I -> invWorld w
        Action J -> actionDirection South w
        Action K -> actionDirection North w
        Action L -> actionDirection West w
        Action N -> actionDirection SouthEast w
        Action Q -> actionMonster $ GA.actionQuaff w
        Action R -> resetWorld w
        Action T -> actionMonster $ GA.actionThrow w
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
