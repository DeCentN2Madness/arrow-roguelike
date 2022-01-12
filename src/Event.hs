{-
Event.hs

Keyboard Handling

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Event where

import Prelude hiding (Left, Right)
import ArrowData
import qualified SDL


actionIntent :: SDL.EventPayload -> Intent
actionIntent SDL.QuitEvent         = Quit
actionIntent (SDL.KeyboardEvent k) = getKey k
actionIntent _                     = Idle

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

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
    SDL.KeycodeH      -> SelectSurface H
    SDL.KeycodeX      -> SelectSurface X
    _                 -> SelectSurface Help

mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (actionIntent . extractPayload)

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
selectSurface H     = _h
selectSurface X     = _x
