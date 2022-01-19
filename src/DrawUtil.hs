{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-

DrawUtil.hs

This module keeps the drawing routines for SDL.Renderer.


Note: (MonadIO m) => SDL.Renderer from Util.withRenderer

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module DrawUtil where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified SDL
import SDL (($=))
import qualified Data.Vector as V
import ArrowData (World(..))
import Dungeon (Dungeon(..), Terrain(..))
import qualified Util as U

data AssetMap a = AssetMap
  { background :: a
  , coin :: a
  , hero :: a
  , light :: a
  , open :: a
  , rubble :: a
  , stairDown :: a
  , stairUp :: a
  , wall :: a
  } deriving (Functor, Foldable, Traversable)

data Colour = White | Red | Blue | Green | Yellow

type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)

assetPaths :: PathMap
assetPaths = AssetMap
  { background = "./assets/Background.png"
  , coin = "./assets/Coin.png"
  , hero = "./assets/Hero.png"
  , light = "./assets/Light.png"
  , open = "./assets/Open.png"
  , rubble = "./assets/Rubble.png"
  , stairDown = "./assets/StairDown.png"
  , stairUp = "./assets/StairUp.png"
  , wall = "./assets/Wall.png"
  }

-- | draw main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r White
  SDL.clear r
  -- Background
  renderTexture r (background ts) (0.0, 0.0 :: Double)
  -- DrawMap
  drawMap r ts w
  -- HUD
  setColor r Green
  SDL.drawRect r (Just hud)
  -- the Hero appears...
  if starting w
    then return ()
    else renderTexture r (hero ts) (midX, midY)
  -- Screen
  SDL.present r
  where
    hud = U.mkRect 0 (height-25) width height
    width = floor (fst $ screenXY w)
    height = floor (snd $ screenXY w)
    midX = ((fst $ screenXY w) - (fst $ scaleXY w)) / 2.0
    midY = ((snd $ screenXY w) - (snd $ scaleXY w)) / 2.0

-- | drawE draws Coord @(x,y)@ wDungeon element.
--- Coord is then translated into the screen with scaleXY and
--  cameraXY
drawE :: (Int, Int)
  -> SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> World
  -> IO ()
drawE (x,y) r t w = do
  renderTexture r t (newX, newY)
  where
    xPos = (fst $ scaleXY w) * (fromIntegral x)
    yPos = (snd $ scaleXY w) * (fromIntegral y)
    newX = xPos - (fst $ cameraXY w)
    newY = yPos - (snd $ cameraXY w)

-- | drawMap
-- apply filters to the Dungeon for display
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let terrainList = V.toList $ dungeonTiles $ dungeon w
      wallList = filter ((== Wall).fst) $ zip terrainList (grid w)
      openList = filter ((== Open).fst) $ zip terrainList (grid w)
      rubbleList = filter ((== Rubble).fst) $ zip terrainList (grid w)
      -- the Hero
      wallT = filter (/= pos) $ [v | (_, v) <- wallList]
      openT = filter (/= pos) $ [v | (_, v) <- openList]
      rubbleT = filter (/= pos) $ [v | (_, v) <- rubbleList]
      pos = (wHero w)
  forM_ wallT $ \i -> drawE i r (wall ts) w
  forM_ rubbleT $ \i -> drawE i r (rubble ts) w
  forM_ openT $ \i -> drawE i r (open ts) w

loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)

type PathMap = AssetMap FilePath

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

setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
