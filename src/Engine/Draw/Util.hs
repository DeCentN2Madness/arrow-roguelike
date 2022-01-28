{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-

Engine.Draw.Util.hs

This module keeps the drawing routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Util where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified SDL
import SDL (($=))
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U
import Game.Dungeon (Terrain(..))
import Game.Kind.Entity (Entity(..))
import qualified Game.Actor as GA
import qualified Game.Tile as GT

data AssetMap a = AssetMap
  { arrow :: a
  , background :: a
  , bang :: a
  , coin :: a
  , hero :: a
  , item :: a
  , magma :: a
  , mouse :: a
  , mushroom :: a
  , open :: a
  , rock :: a
  , rubble :: a
  , stairDown :: a
  , stairUp :: a
  , trap :: a
  , wall :: a
  , zero :: a
  } deriving (Functor, Foldable, Traversable)

data Colour = White | Red | Blue | Green | Yellow

type PathMap = AssetMap FilePath

type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow = "./assets/Arrow.png"
  , background = "./assets/Background.png"
  , bang = "./assets/Bang.png"
  , coin = "./assets/Coin.png"
  , hero = "./assets/Hero.png"
  , item = "./assets/Item.png"
  , magma = "./assets/Magma.png"
  , mouse = "./assets/Mouse.png"
  , mushroom = "./assets/Mushroom.png"
  , open = "./assets/Open.png"
  , rock = "./assets/Rock.png"
  , rubble = "./assets/Rubble.png"
  , stairDown = "./assets/StairDown.png"
  , stairUp = "./assets/StairUp.png"
  , trap = "./assets/trap.png"
  , wall = "./assets/Wall.png"
  , zero = "./assets/zero.png"
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
  -- the HUD
  if starting w
    then renderTexture r (arrow ts) (0.0, 0.0 :: Double)
    else do
      -- HUD
      setColor r Green
      SDL.drawRect r (Just hud)
  -- Screen
  SDL.present r
  where
    hud = U.mkRect 0 (height-25) width height
    width = floor (fst $ screenXY w)
    height = floor (snd $ screenXY w)

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
    (camX, camY) = cameraXY w
    (scaleX, scaleY) = scaleXY w
    xPos = scaleX * fromIntegral x
    yPos = scaleY * fromIntegral y
    newX = xPos - camX
    newY = yPos - camY

-- | drawMap
-- apply filters to the Dungeon for display
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let actorT = GA.fromEntity (entityT w)
      wallT  = GT.fromVisual (gameT w)
      drawT  = filter (\(_, j) -> j `notElem` [v | (_, v) <- actorT]) wallT
      seen   = filter (\(_, j) -> j `elem` fovT w) actorT

  -- draw *, %, :, #, .
  forM_ drawT $ \(i,j) -> case i of
    Magma -> drawE j r (magma ts) w
    Rubble -> drawE j r (rubble ts) w
    Rock -> drawE j r (rock ts) w
    Wall -> drawE j r (wall ts) w
    Open -> drawE j r (open ts) w

  -- draw @, !, $, r, ',', >, < if in fovT
  forM_ seen $ \(i,j) -> case i of
    Actor -> drawE j r (hero ts) w
    Bang -> drawE j r (bang ts) w
    Corpse -> drawE j r (rubble ts) w
    Item -> drawE j r (item ts) w
    Mouse -> drawE j r (mouse ts) w
    Mushroom -> drawE j r (mushroom ts) w
    StairDown -> drawE j r (stairDown ts) w
    StairUp -> drawE j r (stairUp ts) w
    Trap -> drawE j r (trap ts) w

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

setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
