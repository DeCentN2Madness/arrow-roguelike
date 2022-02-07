{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Engine.Arrow.Compass
import Engine.Arrow.Data
import qualified Engine.Arrow.View as EAV
import qualified Engine.Draw.Camera as EDC
import qualified Game.AI as GAI
import Game.Actor (EntityMap)
import qualified Game.Actor as GA
import qualified Game.Combat as GC
import qualified Game.Inventory as GI
import Game.Kind.Entity (Entity(..), EntityKind(..))
import qualified Game.Tile as GT

-- | action
-- handle the action within the World
action :: Int -> Coord -> World -> World
action ix pos w = let
  -- Look event
  entry = actionLook $ GA.getEntityBy pos (entityT w)
  -- Combat event
  newWorld = GC.mkCombat 0 ix w
  -- final
  final = if last (journal newWorld) == entry
    then journal newWorld
    else journal newWorld ++ [entry]
  in newWorld { journal = final }

-- | actionBump
-- if there is a block...
actionBump :: Coord -> EntityMap -> Int
actionBump pos em = let
  blockList = filter ((==pos).snd) $ GA.fromBlock em
  ix = if null blockList
    then 0
    else fst $ head blockList
  in ix

-- | actionDirection the world will change with @input@
-- Processs:
-- 1. clamp check the grid
-- 2. bumpAction checks Entity w/ block
-- 3. action handles activities in the World
-- 4. moveT checks valid moves
-- 5. updateView will create new FoV
-- 6. updateCamera w/ newWorld
actionDirection :: Direction -> World -> World
actionDirection input w = if starting w
  then let
    run = w { starting = False }
    in EDC.updateCamera run
  else let
    -- oldWorld
    newTick          = tick w + 1
    (_, playerCoord) = GA.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    -- @ bump Combat
    bump             = actionBump clampCoord (entityT w)
    newCoord         = if bump < 1 then clampCoord else playerCoord
    -- newWorld from actionMove $ action
    newWorld         = actionMove $ action bump newCoord w
    (pEntity, _)     = GA.getPlayer (entityT newWorld)
    run = if newCoord `elem` moveT pEntity
      then
      EAV.updateView $ newWorld {
      tick      = newTick
      , fovT    = EAV.mkView newCoord (gameT newWorld)
      , entityT = GA.updatePlayerBy newCoord (entityT newWorld)
      , dirty   = True }
      else newWorld { tick = newTick, dirty = False }
  in EDC.updateCamera run

-- | actionGet
-- if there is something to pickup...
-- TODO pickup
actionGet :: World -> World
actionGet w = let
  (pEntity, pPos) = GA.getPlayer (entityT w)
  items = GA.getEntityBy pPos (entityT w)
  newPlayer = if not (null items)
    then GI.pickup items pEntity
    else pEntity
  entry = case items of
    [x] -> T.pack $ "Get id=" ++ show x ++ ", ..."
    _   -> T.pack "Nothing..."
  -- newWorld
  newWorld = w { entityT = GA.updatePlayer newPlayer (entityT w)
               , journal = journal w ++ [entry] }
  in newWorld

-- | actionLook
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if kind ek /= Actor then
                          T.pack $ show (kind ek) ++ ", " else "" ]
  in T.append look "..."

-- | actionMove
-- Where can the Entity move? And then move the Entity.
actionMove :: World -> World
actionMove w = let
  coordF :: [Coord] -> [Coord]
  coordF xs = filter (`notElem` blockT) $ filter (`notElem` hardT) xs
  (_, pPos) = GA.getPlayer (entityT w)
  hardT    = [ xy | (_, xy) <- GT.fromHard (gameT w) ]
  blockT   = [ xy | (_, xy) <- GA.fromBlock (entityT w) ]
  moveList = [ (i, ek) | (e, i) <- GA.fromEntityAt (entityT w),
               let ek = if block e -- Movable Entity
                     then let
                     move = coordF $ cardinal (coord e)
                     pos  = GAI.pathFinder pPos move e
                     in e { coord = pos, moveT = move }
                     else e ]
  in w { entityT = Map.fromList moveList }

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
    Action C -> showCharacter w
    Action G -> actionGet w
    Action I -> showInventory w
    Action R -> resetWorld w
    Quit -> quitWorld w
    _ -> w
  in newWorld

-- | reset the world and redraw the World
-- handle gameStates of restarting...
resetWorld :: World -> World
resetWorld w = let
  g = gameGen w
  (width, height) = screenXY w
  (row, col) = gridXY w
  in mkWorld g (floor width, floor height) row col

-- | showCharacter
showCharacter :: World -> World
showCharacter w = let
  (pEntity, _) = GA.getEntityAt 0 (entityT w)
  pProp = property pEntity
  pStr = read $ Map.findWithDefault "1" "str" pProp :: Int
  pDex = read $ Map.findWithDefault "1" "dex" pProp :: Int
  pCon = read $ Map.findWithDefault "1" "con" pProp :: Int
  pInt = read $ Map.findWithDefault "1" "int" pProp :: Int
  pWis = read $ Map.findWithDefault "1" "wis" pProp :: Int
  pEntry = T.pack $ "Player"
    ++ " Str="
      ++ show pStr
      ++ ", Dex="
      ++ show pDex
      ++ ", Int="
      ++ show pInt
      ++ ", Con="
      ++ show pCon
      ++ ", Wis="
      ++ show pWis
      ++ ", HP="
      ++ show (hitPoint pEntity)
  in w { journal = journal w ++ [pEntry] }

-- | showInventory
showInventory :: World -> World
showInventory w = let
  (pEntity, _) = GA.getEntityAt 0 (entityT w)
  pInv = inventory pEntity
  pEntry = T.pack $ "Inventory=" ++ show pInv
  in w { journal = journal w ++ [pEntry] }

-- | quitWorld
-- handle exiting...
quitWorld :: World -> World
quitWorld w = w { journal = journal w ++ ["Exiting..."]
                  , exiting = True }
