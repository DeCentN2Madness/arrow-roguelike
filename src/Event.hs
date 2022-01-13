{-
Event.hs

Keyboard Handling

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
-}
module Event where

import Prelude hiding (Left, Right)
import qualified SDL
import ArrowData


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
    SDL.KeycodeUp     -> Action Up
    SDL.KeycodeDown   -> Action Down
    SDL.KeycodeLeft   -> Action Left
    SDL.KeycodeRight  -> Action Right
    SDL.KeycodeH      -> Action H
    SDL.KeycodeX      -> Action X
    SDL.KeycodeW      -> Action W
    _                 -> Action Help

mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (actionIntent . extractPayload)

runIntent :: (Monad m) => ActionMap a -> (a -> m ()) -> Intent -> m Bool
runIntent  _ _ Quit = pure False
runIntent  _ _ Idle = pure True
runIntent cs f (Action key) = True <$ f (selectSurface key cs)

selectSurface :: Direction -> ActionMap a -> a
selectSurface Help  = help
selectSurface H     = _h
selectSurface X     = _x
selectSurface _     = _h
