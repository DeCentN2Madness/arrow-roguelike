{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-

Engine.Draw.Visual.hs

This module makes the visual Map

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
  , style      :: a
  } deriving (Functor, Foldable, Traversable)

type PathMap = AssetMap FilePath
type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)
type VisualMap = Map (Int, Int) Visual

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
  | VArrow
  | VWall
  | VLWall
  | VLOpen
  | VLRock
  | VLMagma
  | VLRubble
  | VSpider
  | VPerson
  | VPoison
  | VFire
  | VCold
  | VDragon
  | VWolf
  | VSkeleton
  | VOrc
  | VTroll
  | VHuman
  | VDoor
  | V1
  | V2
  deriving (Show, Eq, Ord)

assetPaths :: PathMap
assetPaths = AssetMap
  { arrow = "./assets/Arrow.png"
  , style = "./assets/ArrowSheet.png"
  }

-- | loadTextures
loadTextures :: (MonadIO m)
  => SDL.Renderer
  -> PathMap
  -> m TextureMap
loadTextures r = mapM (U.loadTextureWithInfo r)

-- | tile sizes
width, height :: CInt
(width, height) = (32, 35)

-- | mkVisual
-- width x height is Tile coordinates which look good to the screen
mkVisual :: VisualKind -> TextureMap -> Visual
mkVisual VActor    ts = Visual (0,   0) (style ts) width height
mkVisual VWall     ts = Visual (32,  0) (style ts) width height
mkVisual VOpen     ts = Visual (64,  0) (style ts) width height
mkVisual VRubble   ts = Visual (96,  0) (style ts) width height
mkVisual VMouse    ts = Visual (128, 0) (style ts) width height
mkVisual VRock     ts = Visual (160, 0) (style ts) width height
mkVisual VMagma    ts = Visual (192, 0) (style ts) width height
mkVisual VMushroom ts = Visual (224, 0) (style ts) width height
mkVisual VPotion   ts = Visual (256, 0) (style ts) width height
mkVisual VStairDn  ts = Visual (288, 0) (style ts) width height
mkVisual VStairUp  ts = Visual (320, 0) (style ts) width height
mkVisual VTrap     ts = Visual (352, 0) (style ts) width height
mkVisual VCoin     ts = Visual (384, 0) (style ts) width height
mkVisual VItem     ts = Visual (416, 0) (style ts) width height
mkVisual VArrow    ts = Visual (448, 0) (style ts) width height
mkVisual VLWall    ts = Visual (480, 0) (style ts) width height
mkVisual VLOpen    ts = Visual (512, 0) (style ts) width height
mkVisual VLRock    ts = Visual (0,   height) (style ts) width height
mkVisual VLMagma   ts = Visual (32,  height) (style ts) width height
mkVisual VLRubble  ts = Visual (64,  height) (style ts) width height
mkVisual VCorpse   ts = Visual (64,  height) (style ts) width height
mkVisual VSpider   ts = Visual (96,  height) (style ts) width height
mkVisual VPerson   ts = Visual (128, height) (style ts) width height
mkVisual VPoison   ts = Visual (160, height) (style ts) width height
mkVisual VFire     ts = Visual (192, height) (style ts) width height
mkVisual VCold     ts = Visual (224, height) (style ts) width height
mkVisual VDragon   ts = Visual (256, height) (style ts) width height
mkVisual VWolf     ts = Visual (288, height) (style ts) width height
mkVisual VSkeleton ts = Visual (320, height) (style ts) width height
mkVisual VOrc      ts = Visual (352, height) (style ts) width height
mkVisual VTroll    ts = Visual (384, height) (style ts) width height
mkVisual VHuman    ts = Visual (416, height) (style ts) width height
mkVisual VDoor     ts = Visual (448, height) (style ts) width height
mkVisual V1        ts = Visual (480, height) (style ts) width height
mkVisual V2        ts = Visual (512, height) (style ts) width height

-- | mkVisualMao
-- Make the visual for rendering
-- 1. Terrain
-- 2. Lit Terrain with Lamp effect in FoV
-- 3. Entities in FoV
--    a. Identify by Name...
--    b. fromEntityStack preserves Actor and Monster...
mkVisualMap :: TextureMap -> World -> VisualMap
mkVisualMap ts w = do
  let entity = GE.fromEntityStack (entityT w)
      walls  = GT.fromVisual (gameT w)
      lit    = filter (\(_, j) -> j `elem` fovT w) walls
      seen   = filter (\(_, j) -> j `elem` fovT w) entity
      -- draw Terrain if visible
      hardT = [ (xy, t) | (tk, xy) <- walls,
                let t = case tk of
                      Door   -> mkVisual VDoor   ts
                      Magma  -> mkVisual VMagma  ts
                      Open   -> mkVisual VOpen   ts
                      Rock   -> mkVisual VRock   ts
                      Rubble -> mkVisual VRubble ts
                      Wall   -> mkVisual VWall   ts ]
      -- draw Terrain if lit
      litT =  [ (xy, t) | (tk, xy) <- lit,
                let t = case tk of
                      Door   -> mkVisual VDoor    ts
                      Magma  -> mkVisual VLMagma  ts
                      Open   -> mkVisual VLOpen   ts
                      Rock   -> mkVisual VRock    ts
                      Rubble -> mkVisual VLRubble ts
                      Wall   -> mkVisual VLWall   ts ]
      -- draw Entities if in fovT
      seenT = [ (xy, t) | (ek, xy) <- seen,
                let t = case kind ek of
                      Actor     -> mkVisual VActor    ts
                      Arrow     -> mkVisual VArrow    ts
                      Coin      -> mkVisual VCoin     ts
                      Corpse    -> mkVisual VCorpse   ts
                      Item      -> mkVisual VItem     ts
                      Mushroom  -> mkVisual VMushroom ts
                      Monster   -> identify ek ts
                      Potion    -> mkVisual VPotion   ts
                      StairDown -> mkVisual VStairDn  ts
                      StairUp   -> mkVisual VStairUp  ts
                      Trap      -> mkVisual VTrap     ts ]
   in Map.fromList $ concat [hardT, litT, seenT]

-- | identify Item, Monster, ... by Name
identify :: EntityKind -> TextureMap -> Visual
identify ek ts = let
  eProp = property ek
  name = Map.findWithDefault "M" "Name" eProp
  vt = case name of
    "Dragon" -> VDragon
    "Mouse"  -> VMouse
    "Orc"    -> VOrc
    "Spider" -> VSpider
    "Troll"  -> VTroll
    "Wolf"   -> VWolf
    _        -> VMouse
  in mkVisual vt ts
