{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Draw.Textual.hs

This module keeps the Text routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Textual where

import Control.Monad (forM_)
import qualified SDL
import qualified SDL.Font
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

red :: SDL.Font.Color
red = SDL.V4 255 0 0 255

green :: SDL.Font.Color
green = SDL.V4 0 255 0 255

blue :: SDL.Font.Color
blue = SDL.V4 0 0 255 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255

-- | drawText Textual the last 3 entries
drawText :: SDL.Renderer -> World -> IO ()
drawText r w = do
  let logs = zip [0..2] $ reverse $ journal w
  forM_ logs $ \(i,j) -> do
    -- Text
    fn <- SDL.Font.load "./assets/fonts/Source_Code_Pro_for_Powerline.otf" 16
    tx <- SDL.Font.blended fn black j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    SDL.Font.free fn
    let hudT = snd (screenXY w) - fromIntegral (snd sz  + (i * snd sz))
    renderText r rt sz (5, hudT)

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
