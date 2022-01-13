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
    SDL.KeycodeA      -> Action A
    SDL.KeycodeD      -> Action D
    SDL.KeycodeE      -> Action E
    SDL.KeycodeQ      -> Action Q
    SDL.KeycodeR      -> Action R
    SDL.KeycodeS      -> Action S
    SDL.KeycodeW      -> Action W
    _                 -> Action Help

mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (actionIntent . extractPayload)
