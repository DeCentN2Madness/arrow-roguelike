{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Prelude hiding (Left, Right)
import Control.Monad.Extra (whileM)
import qualified SDL
import qualified Util as U

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 480)

data Intent
  = SelectSurface Direction
  | Idle
  | Quit

data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  | X
  | Q

data SurfaceMap a = SurfaceMap
  { help :: a
    , up :: a
    , down :: a
    , left :: a
    , right :: a
    , x :: a
    , q :: a
  } deriving (Foldable, Traversable, Functor)

surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap
  { help = "./assets/press.bmp"
    , up = "./assets/up.bmp"
    , down = "./assets/down.bmp"
    , left = "./assets/left.bmp"
    , right = "./assets/right.bmp"
    , x = "./assets/x.bmp"
    , q = "./assets/hello_world.bmp"
  }

{-
    image <- SDL.loadBMP "x.bmp"
    let doRender = U.renderSurfaceToWindow w screen image
    whileM $
      U.isContinue <$> SDL.pollEvent >>= U.conditionallyRun doRender
-}
main :: IO ()
main = U.withSDL $ U.withWindow "Arrow" (screenWidth, screenHeight) $
  \w -> do
    screen <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP surfacePaths
    let doRender = U.renderSurfaceToWindow w screen
    doRender (help surfaces)
    whileM $
      mkIntent <$> SDL.pollEvent
      >>= runIntent surfaces doRender
    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen

{- Keyboard Handling -}
mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (payloadToIntent . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToIntent :: SDL.EventPayload -> Intent
payloadDoIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle

getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> SelectSurface Up
    SDL.KeycodeDown   -> SelectSurface Down
    SDL.KeycodeLeft   -> SelectSurface Left
    SDL.KeycodeRight  -> SelectSurface Right
    SDL.KeycodeX      -> SelectSurface X
    SDL.KeycodeQ      -> SelectSurface Q
    _                 -> SelectSurface Help

runIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m Bool
runIntent _ _ Quit = pure False
runIntent _ _ Idle = pure True
runIntent cs f (SelectSurface key) = True <$ f (selectSurface key cs)

selectSurface :: Direction -> SurfaceMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right
selectSurface X     = x
selectSurface Q     = q
