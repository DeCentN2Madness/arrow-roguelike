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
-- 1. clampCoord checks the input vs grid
-- 2. actionPlayer handles @ events in the world
-- 3. actionMonster handles M events in the World
-- 4. if @ moved then update FoV and Camera
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
    -- newWorld from Monster && Player Events
    newWorld         = actionMonster $ actionPlayer clampCoord w
    (_, pPos) = GP.getPlayer (entityT newWorld)
    run = if pPos == clampCoord
      then
      EAV.updateView $ newWorld {
      tick      = newTick
      , fovT    = EAV.mkView pPos (gameT newWorld)
      , dirty   = True }
      else newWorld { tick = newTick, dirty = False }
  in EDC.updateCamera run

-- | actionGet
-- if there is something to get...
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
    then let
      in T.append "Get " (actionLook $ tail items)
    else T.pack "..."
  in w { tick = newTick
         , entityT = GP.updatePlayer newPlayer newEntity
         , journalT = GJ.updateJournal [entry] (journalT w) }

-- | actionHear
-- if there is something to hear...
actionHear :: EntityMap -> Coord -> Text
actionHear em listen = let
  hear = T.concat [ t | (ix, pos) <- GE.fromBlock em,
                    let t = if ix > 0 && (d > 4 && d < 7)
                          then T.pack "Something moved, "
                          else ""
                        d = GAI.chessDist pos listen ]
  in T.append hear "..."

-- | actionLook
-- if there is something to see...
actionLook :: [(EntityKind, Coord)] -> Text
actionLook xs = let
  look = T.concat [ t | (ek, _) <- xs,
                    let t = if not (block ek) then
                          T.pack $ show (kind ek) ++ ", " else "" ]
  in T.append look "..."

-- | actionMonster
-- 1. Where can the Entity move in relation to Terrain
--    a. also applies to Player
-- 2. pathFinder based on Entities
-- 3. aiAction based on newWorld
actionMonster :: World -> World
actionMonster w = let
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
-- handle the Player within the World
actionPlayer :: Coord -> World -> World
actionPlayer pos w = let
  (pEntity, pPos) = GP.getPlayer (entityT w)
  -- Bump event
  bump = actionBump pos (entityT w)
  bumpCoord = if bump < 1 then pos else pPos
  -- Move event
  newCoord   = if bumpCoord `elem` moveT pEntity then bumpCoord else pPos
  moveWorld  = w { entityT = GP.updatePlayerBy newCoord (entityT w) }
  -- Look event
  look = actionLook $ GE.getEntityBy newCoord (entityT moveWorld)
  -- Hear event
  listen = actionHear (entityT moveWorld) newCoord
  -- Combat event
  newWorld = if bump > 0 then GC.mkCombat 0 bump moveWorld else moveWorld
  -- XP event
  (player, _) = GP.getPlayer (entityT newWorld)
  learn = if eLvl player > eLvl pEntity
    then T.pack $ "Welcome to Level " ++ show (eLvl player) ++ "..."
    else "..."
  in newWorld {
  journalT = GJ.updateJournal [look, listen, learn] (journalT newWorld) }

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

-- | resetWorld, save the Player, and redraw the World
resetWorld :: World -> World
resetWorld w = let
  g = mkStdGen (tick w)
  (width, height) = screenXY w
  (row, col) = gridXY w
  (pEntity, _) = GP.getPlayer (entityT w)
  newWorld = mkWorld g (floor width, floor height) row col
  in newWorld {
  entityT = GE.safeInsertEntity 0 pEntity (gameT newWorld) (entityT newWorld) }

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
      ++ "Lvl="
      ++ show (eLvl pEntity)
      ++ ", Str="
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
  pUnk   = Map.findWithDefault 0 "Unknown"   pInv
  pEntry = T.pack $ "@ "
    ++ "Coin="
    ++ show pCoin
    ++ ", Mushroom="
    ++ show pMush
    ++ ", Potion="
    ++ show pPot
    ++ ", Unk="
    ++ show pUnk
    ++ ", Exp="
    ++ show (eXP pEntity)
  in w { journalT = GJ.updateJournal [pEntry] (journalT w) }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { journalT = GJ.updateJournal ["Saving Game..."] (journalT w)
                , exiting = True }
