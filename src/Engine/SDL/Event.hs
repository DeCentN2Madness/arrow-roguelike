{-

Engine.SDL.Event.hs

Keyboard Handling

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.SDL.Event (mkIntents) where

import qualified SDL
import Engine.Arrow.Data (Direction(..), Intent(..))

actionIntent :: SDL.EventPayload -> Intent
actionIntent SDL.QuitEvent         = Quit
actionIntent (SDL.KeyboardEvent k) = getKey k
actionIntent _                     = Idle

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

-- | getkey
-- vi movement
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
    SDL.KeycodeK      -> Action North
    SDL.KeycodeY      -> Action NorthEast
    SDL.KeycodeH      -> Action East
    SDL.KeycodeN      -> Action SouthEast
    SDL.KeycodeJ      -> Action South
    SDL.KeycodeB      -> Action SouthWest
    SDL.KeycodeL      -> Action West
    SDL.KeycodeU      -> Action NorthWest
    SDL.KeycodeC      -> Action C
    SDL.KeycodeE      -> Action E
    SDL.KeycodeG      -> Action G
    SDL.KeycodeI      -> Action I
    SDL.KeycodeQ      -> Action Q
    SDL.KeycodeR      -> Action R
    _                 -> Action Help

-- TODO handle multiple events
-- <https://github.com/haskell-game/sdl2/issues/241>
mkIntents :: [SDL.Event] -> [Intent]
mkIntents = foldr (\x -> (:) (actionIntent . extractPayload $ x)) [Idle]
