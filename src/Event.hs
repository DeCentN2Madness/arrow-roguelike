{-

Event.hs

Keyboard Handling

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Event where

import qualified SDL
import ArrowData (Direction(..), Intent(..))

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
    SDL.KeycodeUp     -> Action North
    SDL.KeycodeDown   -> Action South
    SDL.KeycodeLeft   -> Action East
    SDL.KeycodeRight  -> Action West
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
