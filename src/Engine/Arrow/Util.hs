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
import System.Random (mkStdGen)
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
--
-- 1. clamp check the grid
-- 2. actionBump checks Player w/ block
-- 3. actionPlayer handles @ activities in the World
-- 4. actionMove handles Entities activities in the Worl
-- 5. moveT checks valid moves
-- 6. updateView will create new FoV
-- 7. updateCamera w/ newWorld
actionDirection :: Direction -> World -> World
actionDirection input w = if starting w
  then let
    run = w { starting = False }
    in EDC.updateCamera run
  else let
    -- oldWorld
    newTick          = tick w + 1
    (_, playerCoord) = GP.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    -- @ Bump Event
    bump             = actionBump clampCoord (entityT w)
    newCoord         = if bump < 1 then clampCoord else playerCoord
    newWorld         = actionMove $ actionPlayer bump newCoord w
    (pEntity, _)     = GP.getPlayer (entityT newWorld)
    -- newWorld
    run = if newCoord `elem` moveT pEntity
      then
      EAV.updateView $ newWorld {
      tick      = newTick
      , entityT = GP.updatePlayerBy newCoord (entityT newWorld)
      , fovT    = EAV.mkView newCoord (gameT newWorld)
      , dirty   = True }
      else newWorld { tick = newTick, dirty = False }
  in EDC.updateCamera run

-- | actionGet
-- if there is something to pickup...
actionGet :: World -> World
actionGet w = let
  newTick = tick w + 1
  (pEntity, pPos) = GP.getPlayer (entityT w)
  items = GE.getEntityBy pPos (entityT w)
  newPlayer = if not (null items)
    then GI.pickup items pEntity
    else pEntity
  newEntity = if not (null items)
    then GI.emptyBy pPos items (entityT w)
    else entityT w
  entry = if length items > 1
    then T.pack "Get..."
    else T.pack "..."
  in w { tick = newTick
         , entityT = GP.updatePlayer newPlayer newEntity
         , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionLook
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if not (block ek) then
                          T.pack $ "See " ++ show (kind ek) ++ ", " else "" ]
  in T.append look "..."

-- | actionMove
-- 1. Where can the Entity move in relation to Terrain
--    a. also applies to Player
-- 2. pathFinder based on Entities
-- 3. aiAction based on newWorld
actionMove :: World -> World
actionMove w = let
  coordF :: [Coord] -> [Coord]
  coordF = filter (`notElem` hardT)
  hardT      = [ xy | (_, xy) <- GT.fromHard (gameT w) ]
  entityList = [ (ix, ek) | (e, ix) <- GE.fromEntityAt (entityT w),
                 let ek = if block e -- Movable Entity
                       then let
                       move = coordF $ cardinal (coord e)
                       in e { moveT = move }
                       else e ]
  -- move w/ hardT
  newWorld = w { entityT = Map.fromList entityList }
  in GAI.aiAction entityList $ GAI.pathFinder entityList newWorld

-- | actionPlayer
-- handle the player within the World
actionPlayer :: Int -> Coord -> World -> World
actionPlayer ix pos w = let
  -- Look event
  entry = actionLook $ GE.getEntityBy pos (entityT w)
  -- Combat event
  newWorld = GC.mkCombat 0 ix w
  in newWorld { journalT = GJ.updateJournal [entry] (journalT newWorld) }

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

-- | resetWorld and redraw the World
resetWorld :: World -> World
resetWorld w = let
  g = mkStdGen (tick w)
  (width, height) = screenXY w
  (row, col) = gridXY w
  in mkWorld g (floor width, floor height) row col

-- | showCharacter
showCharacter :: World -> World
showCharacter w = let
  (pEntity, _) = GP.getPlayer (entityT w)
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
      ++ show (eHP pEntity)
      ++ "/"
      ++ show (eMaxHP pEntity)
  in w { journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | showInventory
showInventory :: World -> World
showInventory w = let
  (pEntity, _) = GP.getPlayer(entityT w)
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
    ++ ", Exp="
    ++ show (eXP pEntity)
    ++ ", Lvl="
    ++ show (eLvl pEntity)
  in w { journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { journalT = GJ.updateJournal ["Saving Game..."] (journalT w)
                , exiting = True }
