{-

Engine.Arrow.View.hs

FoV to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.View (mkView, updateView) where

import qualified Data.Set as S
import Engine.Arrow.Data
import Engine.Arrow.Compass
import Engine.Arrow.FoV (checkFov)
import Game.Tile (TileMap)
import qualified Game.Tile as GT

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
