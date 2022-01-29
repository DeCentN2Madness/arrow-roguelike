{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Draw.Util.hs

This module keeps the drawing routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Util where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as Map
import qualified SDL
import SDL (($=))
import Engine.Arrow.Data (World(..))
import Engine.Draw.Visual
import qualified Engine.SDL.Util as U

data Colour = White | Red | Blue | Green | Yellow

-- | draw main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r White
  SDL.clear r
  -- Background
  renderTexture r (background ts) (0.0, 0.0 :: Double)
  -- Game
  if starting w
    then renderTexture r (arrow ts) (0.0, 0.0 :: Double)
    else do
     -- Draw World Map
     drawMap r ts w
     -- HUD
     setColor r Green
     SDL.drawRect r (Just hud)
  -- Screen
  SDL.present r
  where
    hud = U.mkRect 0 (height-25) width height
    width = floor (fst $ screenXY w)
    height = floor (snd $ screenXY w)

-- | drawE draws Coord @(x,y)@ Dungeon element.
--- Coord is then translated into the screen with scaleXY and
--  cameraXY
drawE :: (Int, Int)
  -> SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> World
  -> IO ()
drawE (x,y) r t w = do
  renderTexture r t (newX, newY)
  where
    (camX, camY) = cameraXY w
    (scaleX, scaleY) = scaleXY w
    xPos = scaleX * fromIntegral x
    yPos = scaleY * fromIntegral y
    newX = xPos - camX
    newY = yPos - camY

-- | drawMap
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let visual = mkVisualMap ts w
      visualT = [(k, v) | k <- Map.keys visual,
                 let (Just v) = Map.lookup k visual]
  forM_ visualT $ \(i, j) -> drawE i r j w

-- | renderTexture
renderTexture :: (Num a, RealFrac a)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> IO ()
renderTexture r (t, ti) (x, y)
  = SDL.copy r t Nothing (Just $ U.mkRect x' y' a b)
  where
    x' = floor x
    y' = floor y
    a = SDL.textureWidth ti
    b = SDL.textureWidth ti

-- | setColor
setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
