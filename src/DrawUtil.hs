{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-

DrawUtil.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module DrawUtil where

import Control.Monad.IO.Class (MonadIO)
import qualified SDL
import SDL (($=))
import ArrowData (World(..))
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
  --SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  renderTexture r (background ts) (0, 0 :: Double)
  renderTexture r (hero ts) (x, y :: Double)
  renderTexture r (wall ts) (0, 48 :: Double)
  renderTexture r (wall ts) (0, 96 :: Double)
  renderTexture r (stairDown ts) (96, 96 :: Double)
  renderTexture r (stairUp ts) (320, 96 :: Double)
  SDL.present r
  where
    x = (xScale w) * (fromIntegral $ fst (wHero w))
    y = (yScale w) * (fromIntegral $ snd (wHero w))

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
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound

type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
