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
import qualified SDL.Font
import SDL (($=))
import Engine.Arrow.Data (World(..))
import Engine.Draw.Visual
import qualified Engine.SDL.Util as U

data Colour = White | Red | Blue | Green | Yellow

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255

-- | draw main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  let hudHt = snd (screenXY w) - 50
  setColor r White
  SDL.clear r
  -- Background
  renderTexture r (background ts) (0.0, 0.0 :: Double)
  -- Game
  if starting w
    then renderTexture r (arrow ts) (0.0, 0.0 :: Double)
    else do
     -- Draw Visual Map
     drawMap r ts w
     -- HUD
     renderTexture r (hud ts) (0.0, hudHt)
     -- Text
     fn <- SDL.Font.load "./assets/fonts/Source_Code_Pro_for_Powerline.otf" 16
     tx <- SDL.Font.blended fn black (last $ journal w)
     sz <- SDL.Font.size fn (last $ journal w)
     rt <- SDL.createTextureFromSurface r tx
     SDL.Font.free fn
     renderText r rt sz (0.0, hudHt)
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
  renderVisual r vis (newX, newY)
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
  forM_ visualT $ \(i, j) -> drawCamera i r j w

-- | renderText
-- write Text to the screen
renderText :: (Num a, RealFrac a)
  => SDL.Renderer
  -> SDL.Texture
  -> (Int, Int)
  -> (a, a)
  -> IO ()
renderText r t (tw, th) (x, y) = let
  rectB = U.mkRect (floor x) (floor y) (fromIntegral tw) (fromIntegral th)
  in SDL.copy r t Nothing (Just  rectB)

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
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
