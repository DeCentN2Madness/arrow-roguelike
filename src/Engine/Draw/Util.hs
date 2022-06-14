{-

Engine.Draw.Util.hs

This module keeps the drawing routines for SDL.Renderer.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Draw.Util where

import Prelude hiding (lookup)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import SDL (($=))
import qualified SDL
import Engine.Arrow.Data (GameState(..), World(..))
import Engine.Draw.Visual (AssetMap(..), TextureMap, Visual(..))
import qualified Engine.Draw.Visual as EDV
import qualified Engine.Draw.Textual as EDT
import qualified Engine.Draw.Inventory as EDI
import qualified Engine.SDL.Util as U
import qualified Game.Player as GP

data Colour
  = Red
  | Green
  | Blue
  | White
  | Black
  | Gray
  | Yellow
  | Brown
  | Purple

-- | draw
-- main drawing loop
draw :: SDL.Renderer -> TextureMap -> World -> IO ()
draw r ts w = do
  setColor r Black
  SDL.clear r
  let pHp    = GP.getHealth (entityT w)
      pMp    = GP.getMana (entityT w)
      pArrow = GP.getArrow (entityT w)
      pMush  = GP.getMushroom (entityT w)
      pPot   = GP.getPotion (entityT w)
  -- Arrow
  if gameState w == GameStart
    then renderTexture r (arrow ts) (0.0, 0.0 :: Double)
    else do
      -- Draw Visual Map
      drawMap r ts w
      -- '@' Stats and Vitals
      renderHpBar r (5, 10)  100.0 180.0 Gray  Gray   1.0
      renderHpBar r (5, 190) 100.0 10.0  Red   Green  pHp
      renderHpBar r (5, 200) 100.0 10.0  White Blue   pMp
      renderHpBar r (5, 210) 100.0 10.0  Gray  Yellow pArrow
      renderHpBar r (5, 220) 100.0 10.0  Gray  Brown  pMush
      renderHpBar r (5, 230) 100.0 10.0  Gray  Purple pPot
      -- HUD Text
      EDT.drawText r w
      -- Look Text
      EDT.drawLook r w
  -- Dialog
  _ <- case gameState w of
    GameDrop      -> EDI.drawInventory r w
    GameEquipment -> EDI.drawEquipment r w
    GameExamine   -> EDI.drawExamine   r w
    GameInventory -> EDI.drawInventory r w
    GameStore     -> EDI.drawStore     r w
    GameSell      -> EDI.drawInventory r w
    _ -> setColor r White
  -- Screen
  SDL.present r
  -- Use `threadDelay` to take advantage of the Haskell runtime.
  threadDelay 100000

-- | drawCamera draws Visual in relation to Camera
--- Coord is then translated into the screen with scaleXY and
--  cameraXY
drawCamera :: (Int, Int)
  -> SDL.Renderer
  -> Visual
  -> World
  -> IO ()
drawCamera (x, y) r vis w = do
  let (camX, camY)     = cameraXY w
      (scaleX, scaleY) = scaleXY w
      xPos             = scaleX * fromIntegral x
      yPos             = scaleY * fromIntegral y
      newX             = xPos - camX
      newY             = yPos - camY
  renderVisual r vis (newX, newY)

-- | drawMap
drawMap :: SDL.Renderer -> TextureMap -> World -> IO ()
drawMap r ts w = do
  let visual = Map.toList $ EDV.mkVisualMap ts w
  forM_ visual $ \(i, j) -> drawCamera i r j w

-- | renderHpBar
-- draw '@' HP
renderHpBar :: (MonadIO m)
  => SDL.Renderer
  -> (Double, Double)
  -> Double
  -> Double
  -> Colour
  -> Colour
  -> Double
  -> m ()
renderHpBar r (x, y) w h bgColour fgColour p = do
  let percent
        | p > 1.0 = p
        | p < 0.0 = 0.0
        | otherwise = p
      bgRect = U.mkRect (floor x) (floor y) (floor w) (floor h)
      fgRect = U.mkRect (floor px) (floor y) (floor pw) (floor h)
      pw     = w * percent
      px     = x + (w - pw)
  setColor r bgColour
  SDL.fillRect r (Just bgRect)
  setColor r fgColour
  SDL.fillRect r (Just fgRect)

-- | renderTexture
-- draw entire texture image
renderTexture :: (Num a, RealFrac a)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> IO ()
renderTexture r (t, ti) (x, y) = let
    rectB = U.mkRect (floor x) (floor y) tw th
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti
    in SDL.copy r t Nothing (Just rectB)

-- | renderVisual
-- draw clip from Visual which has SDL.Texture and Foreign.C.Types (CInt)
renderVisual :: (Num a, RealFrac a)
  => SDL.Renderer
  -> Visual
  -> (a, a)
  -> IO ()
renderVisual r (Visual (xi, yi) (t, _) tw th) (x, y) = let
    rectA = U.mkRect xi yi tw th
    rectB = U.mkRect (floor x) (floor y) tw th
    in SDL.copy r t (Just rectA) (Just rectB)

-- | setColor
setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 255 0 0 255
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 255 0 255
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 255 255
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 255 255 255 255
setColor r Black  = SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
setColor r Gray   = SDL.rendererDrawColor r $= SDL.V4 128 128 128 255
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 255 255 0 255
setColor r Brown  = SDL.rendererDrawColor r $= SDL.V4 165 42 42 255
setColor r Purple = SDL.rendererDrawColor r $= SDL.V4 128 0 128 255
