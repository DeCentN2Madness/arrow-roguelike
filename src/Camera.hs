{-

Camera.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Camera (updateCamera) where

import ArrowData (World(..))

-- | setCamera
setCamera :: Double
  -> Double
  -> (Double, Double)
  -> (Double, Double)
  -> (Double, Double)
setCamera x y (w, h) (scaleX, scaleY) = (newX, newY)
  where
    newX = (x + (scaleX / 2.0)) - (w / 2.0) :: Double
    newY = (y + (scaleY / 2.0)) - (h / 2.0) :: Double

-- | updateCamra
-- keep camera in bounds
updateCamera :: World -> World
updateCamera w = w { cameraXY = newCamera }
  where
    newCamera = setCamera newX newY (screenXY w) (scaleXY w)
    (heroX, heroY) = (wHero w)
    camX = fromIntegral heroX * (fst $ scaleXY w)
    camY = fromIntegral heroY * (snd $ scaleXY w)
    horiz i = max 0 (min i (fst $ levelXY w))
    vert  j = max 0 (min j (snd $ levelXY w))
    newX = horiz camX
    newY = vert camY
