{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-

Engine.Draw.Visual.hs

This module keeps the visual Map

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Visual (assetPaths
                          , loadTextures
                          , mkVisualMap
                          , AssetMap(..)
                          , TextureMap
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
  { arrow :: a
  , background :: a
  , bang :: a
  , coin :: a
  , corpse :: a
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

type Coord = (Int, Int)
type PathMap = AssetMap FilePath
type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
type VisualMap = Map Coord (SDL.Texture, SDL.TextureInfo)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow = "./assets/Arrow.png"
  , background = "./assets/Background.png"
  , bang = "./assets/Bang.png"
  , coin = "./assets/Coin.png"
  , corpse = "./assets/Rubble.png"
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

-- | drawMap
-- apply filters to the Dungeon for display
mkVisualMap :: TextureMap -> World -> VisualMap
mkVisualMap ts w = do
  let actors = GA.fromEntity (entityT w)
      walls  = GT.fromVisual (gameT w)
      --wallT  = filter (\(_, j) -> j `notElem` [v | (_, v) <- actors]) walls
      seen  = pos : filter (\(_, j) -> j `elem` fovT w && j /= snd pos) actors
      --seen  = filter (\(_, j) -> j `elem` fovT w) actors
      pos    = GA.getPlayer (entityT w)

      -- draw *, %, :, #, .
      hardT = [ (xy, t) | (tk, xy) <- walls,
                let t = case tk of
                      Magma -> magma ts
                      Open -> open ts
                      Rock -> rock ts
                      Rubble -> rubble ts
                      Wall -> wall ts
                      ZeroT -> zero ts ]

      -- draw @, !, $, r, ',', >, < if in fovT
      seenT = [ (xy, t) | (tk, xy) <- seen,
                let t = case tk of
                      Actor -> hero ts
                      Bang -> bang ts
                      Corpse -> corpse ts
                      Item -> item ts
                      Mouse -> mouse ts
                      Mushroom -> mushroom ts
                      StairDown -> stairDown ts
                      StairUp -> stairUp ts
                      Trap -> trap ts
                      Unknown -> zero ts ]

    in Map.fromList $ hardT ++ seenT

loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)
