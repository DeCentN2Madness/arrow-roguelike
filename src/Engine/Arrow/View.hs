{-

Engine.Arrow.View.hs

FoV to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.View (mkView, updateView) where

import qualified Data.Set as S
import Engine.Arrow.Data
import Engine.Arrow.FoV (checkFov)
import Game.Tile (TileMap)
import qualified Game.Tile as GT

-- | operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
