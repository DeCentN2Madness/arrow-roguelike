{-

ArrowDataUtil.hs

applyIntent to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module ArrowDataUtil (applyIntent) where

import Data.List (nub)
import qualified Data.Set as S
import ArrowData
import Dungeon (getTerrainAt
               , rogueDungeon
               , Terrain(..))
import Camera (updateCamera)
import GameData (fromHard
                , fromOpen
                , GameMap
                , mkGameMap
                , updateGameMap
                , unionGameMap)
import FoV (checkFov)

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
    Action E -> rotate Clock w
    Action Q -> rotate Counter w
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

-- | dirToDeg @d@ changes degrees
dirToDeg :: Direction -> Int
dirToDeg d
  | d == North = 0
  | d == South = 180
  | d == East  = 90
  | d == West  = 270
  | otherwise  = 0

-- | handleDir @w@ world will change with @input@
-- horiz, vert check the grid
-- getTerrainAt check the dungeon
-- mkView creates the FoV
-- newWorld if movement
handleDir :: Direction -> World -> World
handleDir input w = if (starting w)
    then do
      let openList = fromOpen (gameT w)
          startPos = openList !! 1
      updateCamera w { wHero = startPos, starting = False }
    else do
      let newCoord = (newX, newY)
          heading  = dirToDeg input
          (heroX, heroY) = (wHero w) |+| dirToCoord input
          horiz i = max 0 (min i (fst $ gridXY w))
          vert  j = max 0 (min j (snd $ gridXY w))
          newX = horiz heroX
          newY = vert heroY
          newWorld = case getTerrainAt (newX, newY) (dungeon w) of
            Open -> w {fovT = mkView newCoord (gameT w)
                      , wHero = newCoord
                      , degrees = heading
                      , dirty = True }
            _ -> w { degrees = heading, dirty = False }
      updateCamera $ updateView newWorld

-- | mkView utilizes FoV for @hardT@ to create the visible places
-- defaults (0,0) which  tail $ nub cleans up
mkView :: (Int, Int) -> GameMap -> [Coord]
mkView pos gm =
  let hardT    = fromHard gm
      -- FoV for Open Tiles, not Wall or Rubble
      viewList = S.toList $ checkFov pos hardT 8
      viewT = [ i | v <- viewList,
                let i = case (v `elem` hardT) of
                        True -> (0,0) -- Hard space
                        False -> v    -- actually Open space
              ]
      -- final fovT
      fov = tail $ nub $ filter (/= pos) $
        [ i | v <- viewT,
          let i = if
                fst v > 0 && fst v < 80 &&
                snd v > 0 && snd v < 50
                then v
                else (0,0) -- stay in gridXY
        ]
  in fov

-- | updateView
-- remember what @ has seen...
updateView :: World -> World
updateView w = let
  newView = updateGameMap (wHero w) (gameT w)
  newMap  = unionGameMap newView (gameT w)
  in w { gameT = newMap }

-- | reset
-- reset the world and redraw the dungeon
reset :: World -> World
reset w = let
  (d, g) = rogueDungeon (fst $ gridXY w) (snd $ gridXY w) (gameGen w)
  in w { gameGen = g
       , dungeon = d
       , gameT = mkGameMap d
       , fovT = [(0,0)]
       , degrees = 0
       , starting = True }

-- | rotate
rotate :: RotateDirection -> World -> World
rotate Clock   w = w { degrees = degrees w + 15 }
rotate Counter w = w { degrees = degrees w - 15 }

-- | quitWorld
quitWorld :: World -> World
quitWorld w = w { exiting = True }
