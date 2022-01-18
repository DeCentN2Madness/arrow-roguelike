{-# LANGUAGE OverloadedStrings #-}
{-
Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Main (main) where

import Data.IORef
import Control.Monad.Extra (unless)
import qualified SDL
import ArrowData (World(..))
import qualified ArrowDataUtil as ADU
import qualified Event
import DrawUtil (assetPaths, draw, loadTextures, TextureMap)
import qualified Util as U

width, height :: Int
(width, height) = (640, 480)

main :: IO ()
main = do
  world <- newIORef $ ADU.mkWorld (5, 5) (width, height) 20 15
  U.withSDL $ U.withSDLImage $ do
    U.withWindow "Arrow" (width, height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- loadTextures r assetPaths
      mainLoop world r ts
      mapM_ (SDL.destroyTexture . fst) ts
  q <- readIORef world
  print $ show q
  SDL.quit

-- | mainLoop
-- unless quit
--   1. event handling
--   2. world update
--   3. render world
mainLoop :: IORef World
  -> SDL.Renderer
  -> TextureMap
  -> IO ()
mainLoop world render ts = do
  let s = SDL.pollEvent
  e <- Event.mkIntent <$> s
  modifyIORef world (ADU.applyIntent e)
  q <- readIORef world
  draw render ts q
  unless (exiting q) $ mainLoop world render ts
