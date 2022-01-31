{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-

Engine.Draw.Visual.hs

This module keeps the visual Map

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Visual (assetPaths
                          , AssetMap(..)
                          , loadTextures
                          , mkVisualMap
                          , TextureMap
                          , Visual(..)
                          , VisualMap)  where

import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified SDL
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U
import Game.Dungeon (Terrain(..))
import Game.Kind.Entity (Entity(..))
import qualified Game.Actor as GA
import qualified Game.Tile as GT

data AssetMap a = AssetMap
  { arrow      :: a
  , background :: a
  , hero       :: a
  , open       :: a
  , wall       :: a
  , style      :: a
  } deriving (Functor, Foldable, Traversable)

type Coord = (Int, Int)
type PathMap = AssetMap FilePath
type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
type VisualMap = Map Coord Visual
data Visual = Visual !Coord !(SDL.Texture, SDL.TextureInfo)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow      = "./assets/Arrow.png"
  , background = "./assets/Background.png"
  , hero       = "./assets/Hero.png"
  , open       = "./assets/Open.png"
  , wall       = "./assets/Wall.png"
  , style      = "./assets/ArrowSheet.png"
  }

-- | drawMap
-- apply filters to the Dungeon for display
mkVisualMap :: TextureMap -> World -> VisualMap
mkVisualMap ts w = do
  let actors = GA.fromEntity (entityT w)
      walls  = GT.fromVisual (gameT w)
      seen  = pos : filter (\(_, j) -> j `elem` fovT w && j /= snd pos) actors
      pos    = GA.getPlayer (entityT w)

      -- draw *, %, :, #, .
      hardT = [ (xy, t) | (tk, xy) <- walls,
                let t = case tk of
                      Magma  -> Visual (192, 0)(style ts)
                      Open   -> Visual (64,  0)(style ts)
                      Rock   -> Visual (160, 0)(style ts)
                      Rubble -> Visual (96,  0)(style ts)
                      Wall   -> Visual (32,  0)(style ts)
                      _      -> Visual (64,  0)(style ts) ]

      -- draw @, %, r, ',' if in fov
      seenT = [ (xy, t) | (tk, xy) <- seen,
                let t = case tk of
                      Actor     -> Visual (0,  0)(style ts)
                      Corpse    -> Visual (96, 0)(style ts)
                      Mouse     -> Visual (128, 0)(style ts)
                      Mushroom  -> Visual (224, 0)(style ts)
                      _         -> Visual (64,  0)(style ts) ]
    in Map.fromList $ hardT ++ seenT

loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)
