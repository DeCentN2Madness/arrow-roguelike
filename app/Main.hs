{-# LANGUAGE OverloadedStrings #-}
{-
Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Main where

import Data.IORef
import Control.Monad.Extra (unless)
import qualified SDL
import ArrowData (World(..))
import ArrowDataUtil (mkWorld, applyIntent)
import Event (mkIntent)
import DrawUtil (assetPaths, draw, loadTextures, TextureMap)
import qualified Util as U

width, height :: Int
(width, height) = (640, 480)

main :: IO ()
main = do
  world <- newIORef $ mkWorld (5, 5) (width, height) 16 12
  U.withSDL $ U.withSDLImage $ do
    U.withWindow "Arrow" (width, height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- loadTextures r assetPaths
      mainLoop world r ts
      mapM_ (SDL.destroyTexture . fst) ts
  q <- readIORef world
  print $ show q
  SDL.quit

mainLoop :: IORef World
  -> SDL.Renderer
  -> TextureMap
  -> IO ()
mainLoop world render ts = do
  e <- mkIntent <$> SDL.pollEvent
  modifyIORef world (applyIntent e)
  q <- readIORef world
  _ <- U.isContinue <$> SDL.pollEvent
    >>= U.conditionallyRun (draw render ts q)
  unless (exiting q) $ mainLoop world render ts
