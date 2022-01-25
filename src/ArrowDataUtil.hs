{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module ArrowDataUtil (applyIntent) where

import qualified Data.Set as S
import ArrowData
import Camera (updateCamera)
import Dungeon (Terrain(..))
import qualified Dungeon as D
import GameData (GameMap)
import qualified GameData as GAME
import qualified FoV as FOV

-- | applyIntent
applyIntent :: Intent -> World -> World
applyIntent intent w = let
  newWorld = case intent of
    Action North -> handleDir North w
    Action South -> handleDir South w
    Action East -> handleDir East w
    Action West -> handleDir West w
    Action A ->  handleDir East w
    Action D -> handleDir West w
    Action R -> reset w
    Action S -> handleDir South w
    Action W -> handleDir North w
    Quit -> quitWorld w
    _ -> w
  in newWorld

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | dirToCoord @d@ changes location
dirToCoord :: Direction -> Coord
dirToCoord d
  | d == North = (0, -1)
  | d == South = (0, 1)
  | d == East  = (-1, 0)
  | d == West  = (1, 0)
  | otherwise  = (0, 0)

-- | handleDir @w@ world will change with @input@
-- horiz, vert check the grid
-- getTerrainAt check the dungeon
-- mkView creates the FoV
-- newWorld if movement
handleDir :: Direction -> World -> World
handleDir input w = if (starting w)
    then do
      let em = GAME.insertPlayer (gameT w) (entityT w)
          start = w { entityT = em , starting = False }
      updateCamera start
    else do
      let newCoord = (newX, newY)
          (heroX, heroY) = playerPos |+| dirToCoord input
          horiz i = max 0 (min i (fst $ gridXY w))
          vert  j = max 0 (min j (snd $ gridXY w))
          newX = horiz heroX
          newY = vert heroY
          playerPos  = GAME.findPlayer (entityT w)
          run = case D.getTerrainAt (newX, newY) (dungeon w) of
            Open -> updateView $ w {
              fovT = mkView newCoord (gameT w)
              , entityT = GAME.updatePlayer newCoord (entityT w)
              , dirty = True
              }
            _ -> w { dirty = False }
      updateCamera run

-- | mkView utilizes FoV for @hardT@ to create the visible places
mkView :: (Int, Int) -> GameMap -> [Coord]
mkView pos gm = let
  hardT    = GAME.fromHard gm
  viewList = S.toList $ FOV.checkFov pos hardT 4
  in viewList

-- | updateView, remember what @ has seen...
updateView :: World -> World
updateView w = let
  newMap = GAME.updateGameMap (fovT w) (gameT w)
  in w { gameT = newMap }

-- | reset the world and redraw the dungeon
reset :: World -> World
reset w = let
  (d, g) = D.rogueDungeon (fst $ gridXY w) (snd $ gridXY w) (gameGen w)
  in w { gameGen = g
       , dungeon = d
       , gameT = GAME.mkGameMap d
       , fovT = [(0,0)]
       , starting = True }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
