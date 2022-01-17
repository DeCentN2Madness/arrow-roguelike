{-

Camera.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Camera (updateCamera) where

import ArrowData (World(..))

-- | setCamera
setCamera :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
setCamera x y w h scale = (newX, newY)
  where
    x' = (x + (scale / 2.0)) - (w / 2.0) :: Double
    y' = (y + (scale / 2.0)) - (h / 2.0) :: Double
    newX = if x' > 0 then x' else 0
    newY = if y' > 0 then y' else 0

-- | updateCamra
-- keep camera in bounds
updateCamera :: World -> World
updateCamera w = w { cameraXY = newCamera }
  where
    newCamera = setCamera newX newY (fst $ screenXY w) (snd $ screenXY w) scale
    (heroX, heroY) = (wHero w)
    camX = fromIntegral heroX * (fst $ scaleXY w)
    camY = fromIntegral heroY * (snd $ scaleXY w)
    horiz i = max 0 (min i (fst $ levelXY w))
    vert  j = max 0 (min j (snd $ levelXY w))
    newX = horiz camX
    newY = vert camY
    scale = sum (scaleXY w) / 2.0
