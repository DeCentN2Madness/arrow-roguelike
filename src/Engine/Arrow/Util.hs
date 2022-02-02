{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import qualified Data.Text as T
import Engine.Arrow.Compass
import Engine.Arrow.Data
import qualified Engine.Arrow.View as EAV
import qualified Engine.Draw.Camera as EDC
import Game.Actor (EntityMap)
import qualified Game.Actor as GA
import qualified Game.Combat as GC
import Game.Dungeon (Terrain(..))
import qualified Game.Dungeon as GD
import Game.Kind.Entity (EntityKind(..))
import qualified Game.Tile as GT

-- | action
-- handle the action within the World
action :: Int -> Coord -> World -> World
action ix pos w = let
  -- attack event
  entity = GA.getEntityAt ix (entityT w)
  -- see event
  seen = GA.getEntityBy pos (entityT w)
  entry = if ix > 0
    then T.pack $ "Attack " ++ show (eKind entity) ++ " id=" ++ show ix
    else case seen of
      [e] -> let
        interest = GA.getEntityAt e (entityT w)
        in T.pack $ "See " ++ show (eKind interest) ++ " id=" ++ show e
      _ -> "..."
  -- journal the event
  final = if last (journal w) == entry
    then journal w
    else journal w ++ [entry]
  in GC.mkCombat 0 ix $ w { journal = final }

-- | actionBump
-- make entities block
actionBump :: Coord -> EntityMap -> Int
actionBump pos em = let
  blockList = filter ((==pos).snd) $ GA.fromBlock em
  ix = if null blockList
    then 0
    else fst $ head blockList
  in ix

-- | handleDir @w@ world will change with @input@
-- Processs:
-- 1. clamp check the grid
-- 2. bumpAction checks Entity w/ block
-- 3. action handles activities in the World
-- 4. getTerrainAt checks the TileMap
-- 5. updateView will create new FoV
-- 6. updateCamera w/ newWorld
actionDirection :: Direction -> World -> World
actionDirection input w = if starting w
  then let
    run = w { starting = False }
    in EDC.updateCamera run
  else let
    -- oldWorld
    newTick          = (tick w) + 1
    (_, playerCoord) = GA.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    bump             = actionBump clampCoord (entityT w)
    newCoord         = if bump < 1 then clampCoord else playerCoord
    -- newWorld from action
    newWorld         = action bump newCoord w
    run = case GT.getTerrainAt newCoord (gameT newWorld) of
      Open -> EAV.updateView $ newWorld {
        tick = newTick
        , fovT = EAV.mkView newCoord (gameT newWorld)
        , entityT = GA.updatePlayer newCoord (entityT newWorld)
        , dirty = True }
      _ -> newWorld { dirty = False }
    in EDC.updateCamera run

-- | applyIntent
-- Events applied to the World
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  newWorld = case intent of
    Action North -> actionDirection North w
    Action NorthEast -> actionDirection NorthEast w
    Action East -> actionDirection East w
    Action SouthEast -> actionDirection SouthEast w
    Action South -> actionDirection South w
    Action SouthWest -> actionDirection SouthWest w
    Action West -> actionDirection West w
    Action NorthWest -> actionDirection NorthWest w
    Action R -> resetWorld w
    Quit -> quitWorld w
    _ -> w
  in newWorld

-- | reset the world and redraw the World
-- handle gameStates of starting, dirty, exiting
resetWorld :: World -> World
resetWorld w = let
  (d, g) = uncurry GD.rogueDungeon (gridXY w) (gameGen w)
  tm = GT.mkTileMap d
  em = GA.mkEntityMap tm
  in w { gameGen = g
       , gameT = tm
       , entityT = em
       , fovT = []
       , journal = journal w ++ ["Restarting..."]
       , dirty = True
       , starting = True
       , exiting = False }

-- | quitWorld
-- handle gameStates of starting, dirty, exiting
quitWorld :: World -> World
quitWorld w = w { journal = journal w ++ ["Exiting..."]
                  , starting = False
                  , dirty = False
                  , exiting = True }
