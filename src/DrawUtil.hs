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

data Colour = White | Red | Blue | Green | Yellow

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
  SDL.drawRect r (Just inner)
  -- Hero
  renderTexture r (hero ts) (midX, midY)
  -- Screen
  SDL.present r
  where
    inner = U.mkRect hudX hudY width height
    hudX = 0
    hudY = height - 20
    width = floor (fst $ screenXY w)
    height = floor (snd $ screenXY w)
    midX = ((fst $ screenXY w) - (fst $ scaleXY w)) / 2.0
    midY = ((snd $ screenXY w) - (snd $ scaleXY w)) / 2.0

-- | drawE draws entity based on Coord on grid
--- drawE uses the cameraXY
drawE :: [(Int, Int)]
  -> SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> World
  -> IO ()
drawE [] _ _ _ = return ()
drawE (x:xs) r t w = do
  renderTexture r t (newX, newY)
  drawE xs r t w
  where
    xPos = (fst $ scaleXY w) * (fromIntegral $ fst x)
    yPos = (snd $ scaleXY w) * (fromIntegral $ snd x)
    newX = xPos - (fst $ cameraXY w)
    newY = yPos- (snd $ cameraXY w)

-- | drawMap
-- apply filters to the Dungeon for display
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let terrainList = V.toList $ dungeonTiles $ dungeon w
      wallList = filter ((== Wall).fst ) $ zip terrainList (grid w)
      openList = filter ((== Open).fst ) $ zip terrainList (grid w)
      rubbleList = filter ((== Rubble).fst ) $ zip terrainList (grid w)
      x = filter (/= p) $ [v | (_, v) <- wallList]
      y = filter (/= p) $ [v | (_, v) <- openList]
      z = filter (/= p) $ [v | (_, v) <- rubbleList]
      p = (wHero w)
  drawE x r (wall ts) w
  drawE y r (open ts) w
  drawE z r (rubble ts) w

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

type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
