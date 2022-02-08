{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Draw.Textual.hs

This module keeps the Text routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Textual where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U

-- | drawText Textual the last 3 entries
drawText :: SDL.Renderer -> World -> IO ()
drawText r w = do
  let logs = zip [0..2] $ filter (/="...") $ reverse $ journal w
  fn <- SDL.Font.load "./assets/fonts/Source_Code_Pro_for_Powerline.otf" 16
  forM_ logs $ \(i,j) -> do
    -- Color
    let color x
          | T.any (=='!') x = red
          | T.any (=='@') x = blue
          | otherwise = black
    -- Text
    tx <- SDL.Font.blended fn (color j) j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    -- HUD
    let hudT = snd (screenXY w) - fromIntegral (snd sz  + (i * snd sz))
    renderText r rt sz (5, hudT)
    -- Cleanup
    SDL.freeSurface tx
    SDL.destroyTexture rt
  SDL.Font.free fn

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

red :: SDL.Font.Color
red = SDL.V4 255 0 0 255

green :: SDL.Font.Color
green = SDL.V4 0 255 0 255

blue :: SDL.Font.Color
blue = SDL.V4 0 0 255 255

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255
