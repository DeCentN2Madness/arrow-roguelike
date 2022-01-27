{-

Camera.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Camera (updateCamera) where

import Engine.Arrow.Data (World(..))
import GameData (getPlayer)

-- | setCamera
setCamera :: Double
  -> Double
  -> (Double, Double)
  -> (Double, Double)
  -> (Double, Double)
setCamera x y (w, h) (scaleX, scaleY) = let
  newX = (x + (scaleX / 2.0)) - (w / 2.0) :: Double
  newY = (y + (scaleY / 2.0)) - (h / 2.0) :: Double
  in (newX, newY)

-- | updateCamra
-- keep camera in bounds
updateCamera :: World -> World
updateCamera w = let
  newCamera = setCamera camX camY (screenXY w) (scaleXY w)
  (heroX, heroY) = getPlayer (entityT w)
  camX = fromIntegral heroX * (fst $ scaleXY w)
  camY = fromIntegral heroY * (snd $ scaleXY w)
  in w { cameraXY = newCamera }
