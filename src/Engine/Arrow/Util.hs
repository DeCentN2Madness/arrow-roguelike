{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import qualified Data.Set as S
import qualified Data.Text as T
import Engine.Arrow.Data
import Engine.Arrow.FoV (checkFov)
import Engine.Draw.Camera (updateCamera)
import Game.Actor (EntityMap)
import qualified Game.Actor as GA
import qualified Game.Combat as GC
import Game.Dungeon (Terrain(..))
import qualified Game.Dungeon as DUNGEON
import Game.Kind.Entity (EntityKind(..))
import Game.Tile (TileMap)
import qualified Game.Tile as GT

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | applyIntent
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  newWorld = case intent of
    Action North -> handleDir North w
    Action NorthEast -> handleDir NorthEast w
    Action East -> handleDir East w
    Action SouthEast -> handleDir SouthEast w
    Action South -> handleDir South w
    Action SouthWest -> handleDir SouthWest w
    Action West -> handleDir West w
    Action NorthWest -> handleDir NorthWest w
    Action R -> reset w
    Quit -> quitWorld w
    _ -> w
  in newWorld

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

-- | bumpAction
-- make entities block
bumpAction :: Coord -> EntityMap -> Int
bumpAction pos em = let
  blockList = filter ((==pos).snd) $ GA.fromBlock em
  ix = if null blockList
    then 0
    else fst $ head blockList
  in ix

-- | clamp to grid
cardinal :: Coord -> [Coord]
cardinal pos = let
  coordList = [ pos |+| dirToCoord North
              , pos |+| dirToCoord NorthEast
              , pos |+| dirToCoord East
              , pos |+| dirToCoord SouthEast
              , pos |+| dirToCoord South
              , pos |+| dirToCoord SouthWest
              , pos |+| dirToCoord West
              , pos |+| dirToCoord NorthWest ]
  in coordList

-- | clamp to grid
clamp :: Coord -> Coord -> Coord
clamp (x1, y1) (x2, y2) = let
  horiz i = max 0 (min i x2)
  vert  j = max 0 (min j y2)
  newX = horiz x1
  newY = vert y1
  in (newX, newY)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Coord
dirToCoord d
  | d == North = (0, -1)
  | d == NorthEast = dirToCoord North |+| dirToCoord East
  | d == East  = (-1, 0)
  | d == SouthEast = dirToCoord South |+| dirToCoord East
  | d == South = (0, 1)
  | d == SouthWest = dirToCoord South |+| dirToCoord West
  | d == West  = (1, 0)
  | d == NorthWest = dirToCoord North |+| dirToCoord West
  | otherwise  = (0, 0)

-- | handleDir @w@ world will change with @input@
-- Processs:
-- 1. clamp check the grid
-- 2. bumpAction checks Entity w/ block
-- 3. action handles activities in the World
-- 4. getTerrainAt checks the TileMap
-- 5. updateView will create new FoV
-- 6. updateCamera w/ newWorld
handleDir :: Direction -> World -> World
handleDir input w = if starting w
  then let
    run = w { starting = False }
    in updateCamera run
  else let
    -- oldWorld
    (_, playerCoord) = GA.getPlayer (entityT w)
    (heroX, heroY)   = playerCoord |+| dirToCoord input
    clampCoord       = clamp (heroX, heroY) (gridXY w)
    bump             = bumpAction clampCoord (entityT w)
    newCoord         = if bump < 1 then clampCoord else playerCoord
    -- newWorld from action
    newWorld         = action bump newCoord w
    run = case GT.getTerrainAt newCoord (gameT newWorld) of
      Open -> updateView $ newWorld {
        fovT = mkView newCoord (gameT newWorld)
        , entityT = GA.updatePlayer newCoord (entityT newWorld)
        , dirty = True }
      _ -> newWorld { dirty = False }
    in updateCamera run


-- | mkView utilizes FoV for @hardT@ to create the visible places
mkView :: Coord -> TileMap -> [Coord]
mkView pos gm = let
  hardT    = [ xy | (_, xy) <- GT.fromHard gm ]
  viewList = S.toList $ checkFov pos hardT 4
  coordList = cardinal pos
  in viewList ++ coordList

-- | updateView, remember what @ has seen...
-- clamp fovT to grid
updateView :: World -> World
updateView w = let
  newFov = [ xy | v <- fovT w, let xy = clamp v (gridXY w) ]
  newMap = GT.updateTileMap newFov (gameT w)
  in w { gameT = newMap, fovT = newFov }

-- | reset the world and redraw the dungeon
-- handle gameStates of starting, dirty, exiting
reset :: World -> World
reset w = let
  (d, g) = uncurry DUNGEON.rogueDungeon (gridXY w) (gameGen w)
  tm = GT.mkTileMap d
  em = GA.mkEntityMap tm g
  in w { gameGen = g
       , dungeon = d
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
