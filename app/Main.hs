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
      tx <- U.loadTextureWithInfo r "./assets/Hero.png"
      mainLoop world w r tx
      SDL.destroyTexture (fst tx)

mainLoop :: IORef World
  -> SDL.Window
  -> SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> IO ()
mainLoop world w r tx = do
  e <- E.mkIntent <$> SDL.pollEvent
  modifyIORef world (applyIntent e)
  q <- readIORef world
  print $ q
  _ <- U.isContinue <$> SDL.pollEvent
    >>= U.conditionallyRun (renderWorld r tx q)
  unless (exiting q) $ mainLoop world w r tx

renderWorld :: (MonadIO m)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> World
  ->  m ()
renderWorld r (t, ti) a = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  SDL.copyEx r t (Just mask) (Just pos) deg Nothing flips
  SDL.present r
  where
    tw :: Double
    th :: Double
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti
    s :: SDL.Rectangle Double
    s = U.mkRect 0 0 640 680
    w = U.mkRect 0 0 tw th
    mask = floor <$> s
    pos = floor <$> centerWithin w s
    deg = fromIntegral $ degrees a
    flips = uncurry SDL.V2 (flipped a)

centerWithin :: (Fractional a)
  => SDL.Rectangle a
  -> SDL.Rectangle a
  -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz
  where
    p = SDL.P $ op * (oz - iz) / 2
