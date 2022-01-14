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

draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r White
  SDL.clear r
  renderTexture r (background ts) (0, 0 :: Double)
  renderTexture r (hero ts) (x, y :: Double)
  drawWalls r ts w
  -- renderTexture r (wall ts) (0, 48 :: Double)
  -- renderTexture r (stairDown ts) (96, 96 :: Double)
  -- renderTexture r (stairUp ts) (320, 96 :: Double)
  SDL.present r
  where
    x = (xScale w) * (fromIntegral $ fst (wHero w))
    y = (yScale w) * (fromIntegral $ snd (wHero w))

drawWalls :: SDL.Renderer -> TextureMap -> World -> IO ()
drawWalls r ts w = do
  let d = dungeon w
      (px, py) = wHero w
      terrainList = V.toList $ dungeonTiles d
      width = dungeonWidth d
      enumeratedTerrain = zip [0..] terrainList
      updateList = map (\(i, t) -> let (y, x) = i `divMod` width
                       in if px == x && py == y
                         then False
                         else case t of
                           Open -> False
                           Wall -> True
                           _ -> False) enumeratedTerrain
      coordList = coordTool updateList width (0, 0)
  print $ updateList
  print $ coordList

  renderTexture r (wall ts) (0, 48 :: Double)

coordTool :: [Bool] -> Int -> (Int, Int) -> [(Int, Int)]
coordTool [] _ _  = []
coordTool (x:xs) w (i, j) = (x', y') : coordTool xs w  grid
  where
    (x', y') = if x == True then grid else (i, j)
    grid = if i < w then (i + 1, j) else (0, j + 1)




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
