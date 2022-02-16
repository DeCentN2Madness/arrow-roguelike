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
import qualified Game.AI as GAI
import qualified Game.Entity as GE
import Game.Kind.Entity (Entity(..), EntityKind(..))
import Game.Kind.Tile (Terrain(..))
import qualified Game.Player as GP
import qualified Game.Tile as GT

data AssetMap a = AssetMap
  { arrow      :: a
  , background :: a
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
  | VLWall
  | VLOpen
  | VLRock
  | VLMagma
  | VLRubble
  | VSpider
  | VRMouse
  | VPoison
  | VFire
  | VCold
  | VDragon
  | VWolf
  | VSkeleton
  | VOrc
  | VTroll
  | VHuman
  | V0
  | V1
  | V2
  deriving (Show, Eq, Ord)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow      = "./assets/Arrow.png"
  , background = "./assets/Background.png"
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
mkVisual VLWall    ts = Visual (480, 0) (style ts) 32 36
mkVisual VLOpen    ts = Visual (512, 0) (style ts) 32 36
mkVisual VLRock    ts = Visual (0,  36) (style ts) 32 36
mkVisual VLMagma   ts = Visual (32, 36) (style ts) 32 36
mkVisual VLRubble  ts = Visual (64, 36) (style ts) 32 36
mkVisual VSpider   ts = Visual (96, 36) (style ts) 32 36
mkVisual VRMouse   ts = Visual (128, 36) (style ts) 32 36
mkVisual VPoison   ts = Visual (160, 36) (style ts) 32 36
mkVisual VFire     ts = Visual (192, 36) (style ts) 32 36
mkVisual VCold     ts = Visual (224, 36) (style ts) 32 36
mkVisual VDragon   ts = Visual (256, 36) (style ts) 32 36
mkVisual VWolf     ts = Visual (288, 36) (style ts) 32 36
mkVisual VSkeleton ts = Visual (320, 36) (style ts) 32 36
mkVisual VOrc      ts = Visual (352, 36) (style ts) 32 36
mkVisual VTroll    ts = Visual (384, 36) (style ts) 32 36
mkVisual VHuman    ts = Visual (416, 36) (style ts) 32 36
mkVisual V0        ts = Visual (448, 36) (style ts) 32 36
mkVisual V1        ts = Visual (480, 36) (style ts) 32 36
mkVisual V2        ts = Visual (512, 36) (style ts) 32 36

-- | mkVisualMao
-- make the visual map to render
mkVisualMap :: TextureMap -> World -> VisualMap
mkVisualMap ts w = do
  let entities = GE.fromEntityBy (entityT w)
      walls    = GT.fromVisual (gameT w)
      blockT   = [ xy | (_, xy) <- GE.fromBlock (entityT w) ]
      actors   = filter (\(_, j) -> j `elem` blockT) seen
      lit      = filter (\(_, j) -> j `elem` fovT w) walls
      seen     = filter (\(_, j) -> j `elem` fovT w) entities
      (_,gPos) = GP.getPlayer (entityT w)
      -- draw Terrain if visible
      hardT = [ (xy, t) | (tk, xy) <- walls,
                let t = case tk of
                      Magma  -> mkVisual VMagma  ts
                      Open   -> mkVisual VOpen   ts
                      Rock   -> mkVisual VRock   ts
                      Rubble -> mkVisual VRubble ts
                      Wall   -> mkVisual VWall   ts ]
      -- draw Terrain if lit
      litT = filter ((/=(0,0)).fst) $ [ (xy, t) | (tk, pos) <- lit,
                let t = case tk of
                      Magma  -> mkVisual VLMagma  ts
                      Open   -> mkVisual VLOpen   ts
                      Rock   -> mkVisual VLRock   ts
                      Rubble -> mkVisual VLRubble ts
                      Wall   -> mkVisual VLWall   ts
                    xy = if GAI.adjacent gPos pos then pos else (0,0) ]
      -- draw Entities if in fovT
      seenT = [ (xy, t) | (ek, xy) <- seen,
                let t = case kind ek of
                      Coin      -> mkVisual VCoin     ts
                      Corpse    -> mkVisual VCorpse   ts
                      Item      -> mkVisual VItem     ts
                      Mushroom  -> mkVisual VMushroom ts
                      Potion    -> mkVisual VPotion   ts
                      StairDown -> mkVisual VStairDn  ts
                      StairUp   -> mkVisual VStairUp  ts
                      Trap      -> mkVisual VTrap     ts
                      Unknown   -> mkVisual VUnknown  ts
                      _         -> mkVisual VCorpse   ts ]
      -- draw Actors if in fovT
      actorT = [ t | (ek, xy) <- actors, let t = identify xy ek ts ]
   in Map.fromList $ concat [hardT, litT, seenT, actorT]

-- | identify Monster by Name
identify :: Coord -> EntityKind -> TextureMap -> (Coord, Visual)
identify pos ek ts = let
  eProp = property ek
  v = Map.findWithDefault "0" "Name" eProp
  vt = case v of
    "Mouse"  -> VMouse
    "Orc"    -> VOrc
    "Player" -> VActor
    _        -> VMouse
  in (pos, mkVisual vt ts)
