{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Draw.Textual.hs

This module draws the Journal...

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Textual (drawLook
                           , drawText
                           , renderText) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U
import qualified Game.Journal as GJ
import qualified Game.Player as GP

drawLook :: (MonadIO m) => SDL.Renderer -> World -> m ()
drawLook r w = do
  let view = zip [0..9] $ GP.characterLook (fovT w) (entityT w)
      -- Color
      color x
        | T.any (==':') x = green
        | T.any (=='~') x = yellow
        | T.any (=='!') x = red
        | T.any (=='*') x = purple
        | otherwise = white
  fn <- SDL.Font.load "./assets/fonts/Hack-Regular.ttf" 14
  -- Journal
  forM_ view $ \(i, j) -> do
    -- Text
    tx <- SDL.Font.blended fn (color j) j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    -- HUD
    let hudT = snd (screenXY w) - fromIntegral (snd sz + (i * snd sz))
    renderText r rt sz (700, hudT)
    -- Cleanup
    SDL.freeSurface tx
    SDL.destroyTexture rt
  SDL.Font.free fn

-- | drawText
-- Show the Journal
-- Show the Character
drawText :: (MonadIO m) => SDL.Renderer -> World -> m ()
drawText r w = do
  let logs  = GJ.fromJournal [0..9] (journalT w)
      sheet = zip [0..] $ GP.characterSheet (entityT w)
      -- Color
      color x
        | T.any (==':') x = green
        | T.any (=='~') x = yellow
        | T.any (=='!') x = red
        | T.any (=='*') x = purple
        | T.any (=='-') x = blue
        | otherwise = white
  fn <- SDL.Font.load "./assets/fonts/Hack-Regular.ttf" 14
  -- Journal
  forM_ logs $ \(i, j) -> do
    -- Text
    tx <- SDL.Font.blended fn (color j) j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    -- HUD
    let hudT = snd (screenXY w) - fromIntegral (snd sz + (i * snd sz))
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
renderText :: (MonadIO m)
  => SDL.Renderer
  -> SDL.Texture
  -> (Int, Int)
  -> (Double, Double)
  -> m ()
renderText r t (tw, th) (x, y) = do
  let rectB = U.mkRect (floor x) (floor y) (fromIntegral tw) (fromIntegral th)
  SDL.copy r t Nothing (Just rectB)

-- | colors
red :: SDL.Font.Color
red = SDL.V4 255 0 0 255

green :: SDL.Font.Color
green = SDL.V4 0 255 0 255

blue :: SDL.Font.Color
blue = SDL.V4 0 0 255 255

purple :: SDL.Font.Color
purple = SDL.V4 128 0 128 255

yellow :: SDL.Font.Color
yellow = SDL.V4 255 255 0 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255
