{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (mkStdGen)
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
  entry    = actionLook $ GA.getEntityBy pos (entityT w)
  -- Combat event
  newWorld = GC.mkCombat 0 ix w
  -- final
  final    = if last (journal newWorld) == entry
    then journal newWorld
    else journal newWorld ++ [entry]
  in newWorld { journal = final }

-- | actionBump
-- if there is a block...
actionBump :: Coord -> EntityMap -> Int
actionBump pos em = let
  blockList = filter ((==pos).snd) $ GA.fromBlock em
  in if null blockList
    then 0
    else fst $ head blockList

-- | actionDirection the world will change with @input@
--
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
actionGet :: World -> World
actionGet w = let
  newTick = tick w + 1
  (pEntity, pPos) = GA.getPlayer (entityT w)
  items     = GA.getEntityBy pPos (entityT w)
  newPlayer = if not (null items)
    then GI.pickup items pEntity
    else pEntity
  newEntity = if not (null items)
    then GI.emptyBy pPos items (entityT w)
    else entityT w
  entry     = if length items > 1
    then T.pack "Get..."
    else T.pack "..."
  in w { tick = newTick
         , entityT = GA.updatePlayer newPlayer newEntity
         , journal = journal w ++ [entry] }

-- | actionLook
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if kind ek /= Actor then
                          T.pack $ "See " ++ show (kind ek) ++ ", " else "" ]
  in T.append look "..."

-- | actionMove
-- Where can the Entity move? And then move the Entity.
actionMove :: World -> World
actionMove w = let
  coordF :: [Coord] -> [Coord]
  coordF = filter (`notElem` hardT)
  (_, pPos)  = GA.getPlayer (entityT w)
  hardT      = [ xy | (_, xy) <- GT.fromHard (gameT w) ]
  blockT     = [ xy | (_, xy) <- GA.fromBlock (entityT w) ]
  entityList = [ (ix, ek) | (e, ix) <- GA.fromEntityAt (entityT w),
                 let ek = if block e -- Movable Entity
                       then let
                       move = coordF $ cardinal (coord e)
                       pos  = GAI.pathFinder pPos move blockT e
                       in e { coord = pos, moveT = move }
                       else e ]
  in w { entityT = Map.fromList entityList }

-- | applyIntent
-- Events applied to the World
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  newWorld = case intent of
    Action North     -> actionDirection North w
    Action NorthEast -> actionDirection NorthEast w
    Action East      -> actionDirection East w
    Action SouthEast -> actionDirection SouthEast w
    Action South     -> actionDirection South w
    Action SouthWest -> actionDirection SouthWest w
    Action West      -> actionDirection West w
    Action NorthWest -> actionDirection NorthWest w
    Action C -> showCharacter w
    Action G -> actionGet w
    Action I -> showInventory w
    Action R -> resetWorld w
    Quit     -> quitWorld w
    _ -> w
  in newWorld

-- | reset the world and redraw the World
-- handle gameStates of restarting...
resetWorld :: World -> World
resetWorld w = let
  g = mkStdGen (tick w)
  (width, height) = screenXY w
  (row, col) = gridXY w
  in mkWorld g (floor width, floor height) row col

-- | showCharacter
showCharacter :: World -> World
showCharacter w = let
  (pEntity, _) = GA.getEntityAt 0 (entityT w)
  pProp = property pEntity
  pStr  = Map.findWithDefault "1" "str" pProp
  pDex  = Map.findWithDefault "1" "dex" pProp
  pCon  = Map.findWithDefault "1" "con" pProp
  pInt  = Map.findWithDefault "1" "int" pProp
  pWis  = Map.findWithDefault "1" "wis" pProp
  pEntry = T.pack $ "@ "
      ++ "Str="
      ++ pStr
      ++ ", Dex="
      ++ pDex
      ++ ", Con="
      ++ pCon
      ++ ", Int="
      ++ pInt
      ++ ", Wis="
      ++ pWis
      ++ ", HP="
      ++ show (hitPoint pEntity)
  in w { journal = journal w ++ [pEntry] }

-- | showInventory
showInventory :: World -> World
showInventory w = let
  (pEntity, _) = GA.getEntityAt 0 (entityT w)
  pInv   = inventory pEntity
  pCoin  = Map.findWithDefault 0 "Coin"     pInv
  pMush  = Map.findWithDefault 0 "Mushroom" pInv
  pPot   = Map.findWithDefault 0 "Potion"   pInv
  pEntry = T.pack $ "@ "
    ++ "Coin="
    ++ show pCoin
    ++ ", Mushroom="
    ++ show pMush
    ++ ", Potion="
    ++ show pPot
  in w { journal = journal w ++ [pEntry] }

-- | quitWorld
-- handle exiting...
quitWorld :: World -> World
quitWorld w = w { journal = journal w ++ ["Exiting..."], exiting = True }
