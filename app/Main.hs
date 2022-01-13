{-# LANGUAGE OverloadedStrings #-}
{-
Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Main where

import Data.IORef
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (unless)
import SDL (($=))
import qualified SDL
import ArrowData
import ArrowDataUtil (mkWorld, applyIntent)
import qualified Event as E
import qualified Util as U

width, height :: Int
(width, height) = (640, 480)

main :: IO ()
main = do
  world <- newIORef $ mkWorld (0, 0) (width, height) 48
  U.withSDL $ U.withSDLImage $ do
    U.withWindow "Arrow" (width, height) $ \w ->
      U.withRenderer w $ \r -> do
      ts <- loadTextures r assetPaths
      mainLoop world w r ts
      mapM_ (SDL.destroyTexture . fst) ts

mainLoop :: IORef World
  -> SDL.Window
  -> SDL.Renderer
  -> TextureMap
  -> IO ()
mainLoop world w r ts = do
  e <- E.mkIntent <$> SDL.pollEvent
  modifyIORef world (applyIntent e)
  q <- readIORef world
  -- print $ q
  _ <- U.isContinue <$> SDL.pollEvent
    >>= U.conditionallyRun (draw r ts q)
  unless (exiting q) $ mainLoop world w r ts

draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  renderTexture r (background ts) (0, 0 :: Double)
  renderTexture r (hero ts) (x, y :: Double)
  renderTexture r (wall ts) (0, 48 :: Double)
  renderTexture r (wall ts) (0, 96 :: Double)
  renderTexture r (stairDown ts) (96, 96 :: Double)
  renderTexture r (stairUp ts) (320, 96 :: Double)
  SDL.present r
  where
    x = fromIntegral $ fst (wHero w)
    y = fromIntegral $ snd (wHero w)

loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)

renderTexture :: (Num a, RealFrac a)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> IO ()
renderTexture r (t, ti) (x, y)
  = SDL.copy r t Nothing (Just $ U.mkRect x' y' a b)
  where
    x' = floor x
    y' = floor y
    a = SDL.textureWidth ti
    b = SDL.textureWidth ti
