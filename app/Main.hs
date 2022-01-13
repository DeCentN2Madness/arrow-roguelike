{-# LANGUAGE OverloadedStrings #-}
{-
Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Main where

import Data.IORef
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (unless)
import qualified SDL
import ArrowData
import ArrowDataUtil (mkWorld, handleEvent)
import qualified Event as E
import qualified Util as U

width, height :: Int
(width, height) = (640, 480)

main :: IO ()
main = do
  world <- newIORef $ mkWorld (0,0) (width, height) 48
  U.withSDL $ U.withWindow "Arrow" (width, height) $ \w -> do
    screen <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP actionPaths
    let doRender = U.renderSurfaceToWindow w screen
    doRender (help surfaces)
{-
    whileM $
      E.mkIntent <$> SDL.pollEvent
      >>= E.runIntent surfaces doRender
-}
    mainLoop world w screen surfaces

    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen
    SDL.quit
  output <- readIORef world
  print $ output

mainLoop :: IORef World -> SDL.Window -> SDL.Surface -> ActionMap SDL.Surface -> IO ()
mainLoop world w s ss = do
  pixelFormat <- SDL.surfaceFormat s
  surface <- SDL.convertSurface (_h ss) pixelFormat
  e <- E.mkIntent <$> SDL.pollEvent
  let q = case e of
        Quit -> True
        Idle -> False
        Action _ -> False
  modifyIORef world (handleEvent e)
  _ <- U.isContinue <$> SDL.pollEvent >>=  U.conditionallyRun (draw w s surface)
  unless q $ mainLoop world w s ss

draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
draw w s t = SDL.surfaceBlit t Nothing s Nothing
  >> SDL.updateWindowSurface w

{-
draw2 :: SDL.Renderer -> ActionMap SDL.Surface -> IO ()
draw2 r ts = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound
  SDL.clear r
  renderTexture r (_h ts) (0, 0 :: Double)
  renderTexture r (_h ts) (240, 190 :: Double)
  SDL.present r

renderTexture :: (Num a, RealFrac a) => SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (a,a) -> IO ()
renderTexture r (t, ti) (x, y)
  = SDL.copy r t
  Nothing
  (Just $ U.mkRect x' y' a b)
  where
    x' = floor x
    y' = floor y
    a = SDL.textureWidth ti
    b = SDL.textureHeight ti

-}
