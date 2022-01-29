{-# LANGUAGE OverloadedStrings #-}
{-

Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Main (main) where

import Data.IORef
import Control.Monad.Random (getStdGen)
import Control.Monad.Extra (unless)
import qualified SDL
import Engine.Arrow.Data (mkWorld, World(..))
import Engine.Arrow.Util (applyIntent)
import Engine.Draw.Util (draw)
import Engine.Draw.Visual (assetPaths, loadTextures, TextureMap)
import Engine.SDL.Event (mkIntent)
import qualified Engine.SDL.Util as U

width, height :: Int
(width, height) = (640, 480)

main :: IO ()
main = do
  gen <- getStdGen
  world <- newIORef $ mkWorld gen (width, height) 80 50
  U.withSDL $ U.withSDLImage $ do
    U.withWindow "Arrow" (width, height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- loadTextures r assetPaths
      mainLoop world r ts
      mapM_ (SDL.destroyTexture . fst) ts
  q <- readIORef world
  print $ fovT q
  print $ entityT q
  SDL.quit

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
  let s = SDL.pollEvent
  e <- mkIntent <$> s
  modifyIORef world (applyIntent e)
  q <- readIORef world
  draw render ts q
  unless (exiting q) $ mainLoop world render ts
