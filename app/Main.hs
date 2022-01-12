{-# LANGUAGE OverloadedStrings #-}
{-
Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Main where

import Control.Monad.Extra (whileM)
import qualified SDL
import ArrowData
import qualified Event as E
import qualified Util as U

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  let world = World(0,0)
  U.withSDL $ U.withWindow "Arrow" (screenWidth, screenHeight) $ \w -> do
    screen <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP surfacePaths
    let doRender = U.renderSurfaceToWindow w screen
    doRender (help surfaces)
    whileM $
      E.mkIntent <$> SDL.pollEvent
      >>= E.runIntent surfaces doRender
    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen
    SDL.quit
  print $ world
