{-

Engine.Arrow.Util.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Util (applyIntent) where

import Data.List (nub)
import qualified Data.Set as S
import Engine.Arrow.Data
import Engine.Arrow.FoV (checkFov)
import Engine.Draw.Camera (updateCamera)
import Game.Data (GameMap, EntityMap)
import Game.Dungeon (Terrain(..))
import qualified Game.Dungeon as DUNGEON
import qualified Game.Data as GAME

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

-- | bumpAction
-- make entities block
bumpAction :: Coord -> EntityMap -> Int
bumpAction pos em = let
  blockList = filter ((==pos).snd) $ GAME.fromBlock em
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
-- 3. getTerrainAt checks the GameMap
-- 4. if Open then create a new FoV and move
--    else no movement
handleDir :: Direction -> World -> World
handleDir input w = if starting w
    then do
      let mice = GAME.insertMouse (gameT w)
          e = GAME.insertPlayer (gameT w) mice
          start = w { entityT = e, starting = False }
      updateCamera start
    else do
      let playerCoord  = GAME.getPlayer (entityT w)
          (heroX, heroY) = playerCoord |+| dirToCoord input
          clampCoord = clamp (heroX, heroY) (gridXY w)
          bumpCoord = bumpAction clampCoord (entityT w)
          newCoord = if bumpCoord < 1 then clampCoord else playerCoord
          entry = logEvent bumpCoord w
          run = case GAME.getTerrainAt newCoord (gameT w) of
            Open -> updateView $ w {
              fovT = mkView newCoord (gameT w)
              , entityT = GAME.updatePlayer newCoord (entityT w)
              , journal = entry
              , dirty = True }
            _ -> w { dirty = False }
      updateCamera run

-- | logevent
logEvent :: Int -> World -> String
logEvent x w = let
  player = GAME.getEntityAt 0 (entityT w)
  entity = GAME.getEntityAt x (entityT w)
  entry = if x > 0
    then "Attacks from " ++ show player ++ " at " ++ show entity ++ "\n"
    else []
  in entry

-- | mkView utilizes FoV for @hardT@ to create the visible places
mkView :: (Int, Int) -> GameMap -> [Coord]
mkView pos gm = let
  hardT    = [ xy | (_, xy) <- GAME.fromHard gm ]
  viewList = S.toList $ checkFov pos hardT 4
  coordList = cardinal pos
  in nub $ viewList ++ coordList

-- | updateView, remember what @ has seen...
-- clamp fovT to grid
updateView :: World -> World
updateView w = let
  newFov = [ xy | k <- fovT w, let xy = clamp k (gridXY w) ]
  newMap = GAME.updateGameMap newFov (gameT w)
  in w { gameT = newMap, fovT = newFov }

-- | reset the world and redraw the dungeon
reset :: World -> World
reset w = let
  (d, g) = uncurry DUNGEON.rogueDungeon (gridXY w) (gameGen w)
  in w { gameGen = g
       , dungeon = d
       , gameT = GAME.mkGameMap d
       , fovT = []
       , starting = True }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
