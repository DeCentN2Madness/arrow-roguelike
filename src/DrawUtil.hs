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
import ArrowData (World(..), Dungeon(..), Terrain(..))
import qualified Util as U

data AssetMap a = AssetMap
  { background :: a
  , hero :: a
  , wall :: a
  , stairDown :: a
  , stairUp :: a
  } deriving (Functor, Foldable, Traversable)

assetPaths :: PathMap
assetPaths = AssetMap
  { background = "./assets/Background.png"
  , hero = "./assets/Hero.png"
  , wall = "./assets/Wall.png"
  , stairDown = "./assets/StairDown.png"
  , stairUp = "./assets/StairUp.png"
  }

data Colour = White | Red | Blue | Green | Yellow

-- | draw main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r White
  SDL.clear r
  -- Background
  renderTexture r (background ts) (0.0, 0.0 :: Double)
  -- Place stair
  drawE [(5,5)] r (stairDown ts) w
  -- Walls
  drawWalls r ts w
  -- Camera
  setColor r Green
  SDL.drawRect r (Just inner)
  -- Hero
  renderTexture r (hero ts) (midX, midY)
  -- Screen
  SDL.present r
  where
    inner = U.mkRect 0 0 width height
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

-- | drawWalls draws all the walls
drawWalls :: SDL.Renderer -> TextureMap -> World -> IO ()
drawWalls r ts w = do
  let terrainList = V.toList $ dungeonTiles $ dungeon w
      coordList = filter ((== Wall).fst ) $ zip terrainList (grid w)
      drawList = [v | (_, v) <- coordList]
  drawE drawList r (wall ts) w

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
