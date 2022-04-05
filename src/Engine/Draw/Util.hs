{-

Engine.Draw.Util.hs

This module keeps the drawing routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Util where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import qualified SDL
import SDL (($=))
import Engine.Arrow.Data (World(..))
import Engine.Draw.Visual (AssetMap(..), TextureMap, Visual(..))
import qualified Engine.Draw.Visual as EDV
import qualified Engine.Draw.Textual as EDT
import qualified Engine.SDL.Util as U
import qualified Game.Player as GP

data Colour = Red | Green | Blue | White | Black | Yellow

-- | draw
-- main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r Black
  SDL.clear r
  -- Game
  if starting w
    then renderTexture r (arrow ts) (0.0, 0.0 :: Double)
    else do
     -- Draw Visual Map
     drawMap r ts w
     -- HUD Text
     EDT.drawText r w
     -- Hp Bar
     let hp = GP.getHealth (entityT w)
     renderHpBar r (5, 176) 100.0 16.0 hp
  -- Screen
  SDL.present r

-- | drawCamera draws Visual in relation to Camera
--- Coord is then translated into the screen with scaleXY and
--  cameraXY
drawCamera :: (Int, Int)
  -> SDL.Renderer
  -> Visual
  -> World
  -> IO ()
drawCamera (x, y) r vis w = do
  let (camX, camY)     = cameraXY w
      (scaleX, scaleY) = scaleXY w
      xPos             = scaleX * fromIntegral x
      yPos             = scaleY * fromIntegral y
      newX             = xPos - camX
      newY             = yPos - camY
  renderVisual r vis (newX, newY)

-- | drawMap
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let visual = Map.toList $ EDV.mkVisualMap ts w
  forM_ visual $ \(i, j) -> drawCamera i r j w

-- | renderHpBar
-- draw '@' HP
renderHpBar :: (MonadIO m)
  => SDL.Renderer
  -> (Double, Double)
  -> Double
  -> Double
  -> Double
  -> m ()
renderHpBar r (x, y) w h p = do
  let percent
        | p > 1.0 = p
        | p < 0.0 = 0.0
        | otherwise = p
      bgRect = U.mkRect (floor x) (floor y) (floor w) (floor h)
      fgRect = U.mkRect (floor px) (floor y) (floor pw) (floor h)
      pw     = w * percent
      px     = x + (w - pw)
  setColor r Red
  SDL.fillRect r (Just bgRect)
  setColor r Green
  SDL.fillRect r (Just fgRect)

-- | renderTexture
-- draw entire texture image
renderTexture :: (Num a, RealFrac a)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> IO ()
renderTexture r (t, ti) (x, y) = let
    rectB = U.mkRect (floor x) (floor y) tw th
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti
    in SDL.copy r t Nothing (Just rectB)

-- | renderVisual
-- draw clip from Visual which has SDL.Texture and Foreign.C.Types (CInt)
renderVisual :: (Num a, RealFrac a)
  => SDL.Renderer
  -> Visual
  -> (a, a)
  -> IO ()
renderVisual r (Visual (xi, yi) (t, _) tw th) (x, y) = let
    rectA = U.mkRect xi yi tw th
    rectB = U.mkRect (floor x) (floor y) tw th
    in SDL.copy r t (Just rectA) (Just rectB)

-- | setColor
setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Black  = SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
