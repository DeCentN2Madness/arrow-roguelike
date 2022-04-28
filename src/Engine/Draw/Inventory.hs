{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Draw.Inventory.hs

This module keeps the Inventory popUp

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Inventory (drawEquipment
                             , drawInventory) where

import Control.Monad (forM_)
import qualified SDL
import qualified SDL.Font
import Engine.Arrow.Data (World(..))
import qualified Engine.Draw.Textual as EDT
import qualified Game.Player as GP

-- | drawEquipment
-- Show the Equipment
drawEquipment :: SDL.Renderer -> World -> IO ()
drawEquipment r w = do
  let logs = zip [0..] $ GP.characterEquipment (entityT w) (assetT w)
  fn <- SDL.Font.load "./assets/fonts/Hack-Regular.ttf" 14
  -- Journal
  forM_ logs $ \(i, j) -> do
    -- Text
    tx <- SDL.Font.blended fn green j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    -- HUD
    let hudT = fromIntegral $ snd sz + (i * snd sz) :: Double
    EDT.renderText r rt sz (125, hudT)
    -- Cleanup
    SDL.freeSurface tx
    SDL.destroyTexture rt
  SDL.Font.free fn

-- | drawInventory
-- Show the Inventory
drawInventory :: SDL.Renderer -> World -> IO ()
drawInventory r w = do
  let logs = zip [0..] $ GP.characterInventory (entityT w) (assetT w)
  fn <- SDL.Font.load "./assets/fonts/Hack-Regular.ttf" 14
  -- Journal
  forM_ logs $ \(i, j) -> do
    -- Text
    tx <- SDL.Font.blended fn white j
    sz <- SDL.Font.size fn j
    rt <- SDL.createTextureFromSurface r tx
    -- HUD
    let hudT = fromIntegral $ snd sz + (i * snd sz) :: Double
    EDT.renderText r rt sz (125, hudT)
    -- Cleanup
    SDL.freeSurface tx
    SDL.destroyTexture rt
  SDL.Font.free fn

-- | colors
green :: SDL.Font.Color
green = SDL.V4 0 255 0 255

white :: SDL.Font.Color
white = SDL.V4 255 255 255 255
