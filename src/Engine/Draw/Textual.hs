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
import qualified Game.Journal as GJ
import qualified Game.Player as GP

-- | drawText
-- Show the Journal
-- Show the Character
drawText :: SDL.Renderer -> World -> IO ()
drawText r w = do
  let logs  = GJ.fromJournal [0..9] (journalT w)
      sheet = zip [0..] $ GP.characterSheet (entityT w)
      -- Color
      color x
        | T.any (=='!') x = red
        | T.any (==':') x = green
        | T.any (=='@') x = blue
        | T.any (=='*') x = purple
        | otherwise = white
  fn <- SDL.Font.load "./assets/fonts/Hack-Regular.ttf" 16
  -- Journal
  forM_ logs $ \(i, j) -> do
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
  -- Character
  forM_ sheet $ \(i, j) -> do
    tx <- SDL.Font.blended fn (color j) j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    let charT = fromIntegral (snd sz + (i * snd sz)) :: Double
    renderText r rt sz (5.0, charT)
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

-- | colors
black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

blue :: SDL.Font.Color
blue = SDL.V4 0 0 255 255

green :: SDL.Font.Color
green = SDL.V4 0 255 0 255

purple :: SDL.Font.Color
purple = SDL.V4 128 0 128 255

red :: SDL.Font.Color
red = SDL.V4 255 0 0 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255
