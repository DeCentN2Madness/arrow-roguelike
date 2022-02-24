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
import Engine.Arrow.Data (World(..))
import Engine.Arrow.Save (loadFile, saveFile)
import Engine.Arrow.Util (applyIntent)
import Engine.Draw.Util (draw)
import Engine.Draw.Visual (assetPaths, loadTextures, TextureMap)
import qualified Engine.SDL.Event as E
import qualified Engine.SDL.Util as U

-- | main
main :: IO ()
main = do
  saveWorld <- loadFile
  world <- newIORef saveWorld
  start <- readIORef world
  let (width, height) = screenXY start
  U.withSDL $ U.withSDLFont $ U.withSDLImage $ do
    U.withWindow "Arrow" (floor width, floor height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- loadTextures r assetPaths
      mainLoop world r ts
      mapM_ (SDL.destroyTexture . fst) ts
  end <- readIORef world
  saveFile end

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
  let intents = E.mkIntents events
  forM_ intents $ \i -> do
    modifyIORef world (applyIntent i)
    d <- readIORef world
    draw render ts d
  unless (exiting q) $ mainLoop world render ts
