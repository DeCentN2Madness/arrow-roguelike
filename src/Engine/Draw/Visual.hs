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
                          , VisualMap) where

import Prelude hiding (lookup)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Foreign.C.Types (CInt)
import qualified SDL
import Engine.Arrow.Data (World(..))
import qualified Engine.SDL.Util as U
import qualified Game.Entity as GE
import Game.Kind.Entity (Entity(..), EntityKind(..))
import Game.Kind.Tile (Terrain(..))
import qualified Game.Tile as GT

data AssetMap a = AssetMap
  { arrow      :: a
  , background :: a
  , hud        :: a
  , style      :: a
  } deriving (Functor, Foldable, Traversable)

type Coord = (Int, Int)
type PathMap = AssetMap FilePath
type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
type VisualMap = Map Coord Visual

-- Visual matches Foreign.C.Types for SDL.copy
data Visual = Visual !(CInt, CInt) !(SDL.Texture, SDL.TextureInfo) !CInt !CInt

data VisualKind
  = VActor
  | VCoin
  | VCorpse
  | VItem
  | VMagma
  | VMouse
  | VMushroom
  | VOpen
  | VPotion
  | VRubble
  | VRock
  | VStairDn
  | VStairUp
  | VTrap
  | VUnknown
  | VWall
  deriving (Show, Eq, Ord)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow      = "./assets/Arrow.png"
  , background = "./assets/Background.png"
  , hud        = "./assets/Hud.png"
  , style      = "./assets/ArrowSheet.png"
  }

-- | loadTextures
loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)

-- | mkVisual
-- 32 x 36 is Tile coordinates which look good to the screen
mkVisual :: VisualKind -> TextureMap -> Visual
mkVisual VActor    ts = Visual (0,   0) (style ts) 32 36
mkVisual VWall     ts = Visual (32,  0) (style ts) 32 36
mkVisual VOpen     ts = Visual (64,  0) (style ts) 32 36
mkVisual VRubble   ts = Visual (96,  0) (style ts) 32 36
mkVisual VCorpse   ts = Visual (96,  0) (style ts) 32 36
mkVisual VMouse    ts = Visual (128, 0) (style ts) 32 36
mkVisual VRock     ts = Visual (160, 0) (style ts) 32 36
mkVisual VMagma    ts = Visual (192, 0) (style ts) 32 36
mkVisual VMushroom ts = Visual (224, 0) (style ts) 32 36
mkVisual VPotion   ts = Visual (256, 0) (style ts) 32 36
mkVisual VStairDn  ts = Visual (288, 0) (style ts) 32 36
mkVisual VStairUp  ts = Visual (320, 0) (style ts) 32 36
mkVisual VTrap     ts = Visual (352, 0) (style ts) 32 36
mkVisual VCoin     ts = Visual (384, 0) (style ts) 32 36
mkVisual VItem     ts = Visual (416, 0) (style ts) 32 36
mkVisual VUnknown  ts = Visual (448, 0) (style ts) 32 36

-- | mkVisualMao
-- make the visual map to render
mkVisualMap :: TextureMap -> World -> VisualMap
mkVisualMap ts w = do
  let actors = GE.fromEntityBy (entityT w)
      walls  = GT.fromVisual (gameT w)
      seen   = filter (\(_, j) -> j `elem` fovT w) actors
      -- draw Terrain if Visible
      hardT = [ (xy, t) | (tk, xy) <- walls,
                let t = case tk of
                      Magma  -> mkVisual VMagma  ts
                      Rock   -> mkVisual VRock   ts
                      Rubble -> mkVisual VRubble ts
                      Wall   -> mkVisual VWall   ts
                      Open   -> mkVisual VOpen   ts ]
      -- draw Items if in fovT
      seenT = [ (xy, t) | (ek, xy) <- seen,
                let t = case kind ek of
                      Actor     -> mkVisual VActor    ts
                      Coin      -> mkVisual VCoin     ts
                      Corpse    -> mkVisual VCorpse   ts
                      Item      -> mkVisual VItem     ts
                      Mouse     -> mkVisual VMouse    ts
                      Mushroom  -> mkVisual VMushroom ts
                      Potion    -> mkVisual VPotion   ts
                      StairDown -> mkVisual VStairDn  ts
                      StairUp   -> mkVisual VStairUp  ts
                      Trap      -> mkVisual VTrap     ts
                      Unknown   -> mkVisual VUnknown  ts ]
      -- draw Actors if in fovT
      actorT = filter ((/=(0,0)).fst) $ [ t | (ek, xy) <- seen,
                 let t = case kind ek of
                       Actor -> (xy, mkVisual VActor ts)
                       Mouse -> (xy, mkVisual VMouse ts)
                       _     -> ((0,0), mkVisual VUnknown ts) ]
   in Map.fromList $ concat [hardT, seenT, actorT]
