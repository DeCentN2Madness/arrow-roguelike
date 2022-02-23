{-

Engine.Arrow.View.hs

FoV to the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.View (mkView, updateView) where

import qualified Data.Set as S
import Engine.Arrow.Data
import qualified Engine.Arrow.FoV as EAF
import Game.Tile (TileMap)
import qualified Game.Tile as GT

-- | mkView utilizes FoV for @hardT@ to create the visible places
mkView :: Coord -> TileMap -> [Coord]
mkView pos gm = let
  visionT = [ xy | (_, xy) <- GT.fromVisionBlocked gm ]
  in S.toList $ EAF.checkFov pos visionT 4

-- | updateView, remember what @ has seen...
updateView :: World -> World
updateView w = w { gameT = GT.updateTileMap (fovT w) (gameT w) }
