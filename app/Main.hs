{-# LANGUAGE OverloadedStrings #-}
{-

Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Main (main) where

import Data.IORef
import Control.Monad (forM_)
import Control.Monad.Extra (unless)
import qualified SDL
import Engine.Arrow.Data (GameState(..), World(..))
import qualified Engine.Arrow.Save as EAS
import qualified Engine.Draw.Util as EDU
import Engine.Draw.Visual (TextureMap)
import qualified Engine.Draw.Visual as EDV
import qualified Engine.SDL.Event as ESE
import qualified Engine.SDL.Util as U
import qualified Game.Arrow as GA

-- | main
main :: IO ()
main = do
  saveWorld <- EAS.loadFile
  world <- newIORef saveWorld
  start <- readIORef world
  let (width, height) = screenXY start
  U.withSDL $ U.withSDLFont $ U.withSDLImage $ do
    U.withWindow "Arrow" (floor width, floor height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- EDV.loadTextures r EDV.assetPaths
      mainLoop world r ts
      mapM_ (SDL.destroyTexture . fst) ts
  end <- readIORef world
  EAS.saveFile end

-- | mainLoop
-- unless exiting
--   1. event handling
--   2. world update
--   3. render world
mainLoop :: IORef World
  -> SDL.Renderer
  -> TextureMap
  -> IO ()
mainLoop world render ts = do
  q <- readIORef world
  events <- SDL.pumpEvents >> SDL.pollEvents
  let intents = ESE.mkIntents events
  forM_ intents $ \i -> do
    modifyIORef world (GA.applyIntent i)
    d <- readIORef world
    EDU.draw render ts d
  SDL.delay 10
  unless (gameState q == GameStop) $ mainLoop world render ts
