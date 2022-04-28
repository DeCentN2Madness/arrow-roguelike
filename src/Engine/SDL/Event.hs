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
    SDL.Keycode0      -> Action Zero
    SDL.Keycode1      -> Action One
    SDL.Keycode2      -> Action Two
    SDL.Keycode3      -> Action Three
    SDL.Keycode4      -> Action Four
    SDL.Keycode5      -> Action Five
    SDL.Keycode6      -> Action Six
    SDL.Keycode7      -> Action Seven
    SDL.Keycode8      -> Action Eight
    SDL.Keycode9      -> Action Nine
    SDL.KeycodeA      -> Action A
    SDL.KeycodeB      -> Action B
    SDL.KeycodeC      -> Action C
    SDL.KeycodeD      -> Action D
    SDL.KeycodeE      -> Action E
    SDL.KeycodeF      -> Action F
    SDL.KeycodeG      -> Action G
    SDL.KeycodeH      -> Action H
    SDL.KeycodeI      -> Action I
    SDL.KeycodeJ      -> Action J
    SDL.KeycodeK      -> Action K
    SDL.KeycodeL      -> Action L
    SDL.KeycodeM      -> Action M
    SDL.KeycodeN      -> Action N
    SDL.KeycodeO      -> Action O
    SDL.KeycodeP      -> Action P
    SDL.KeycodeQ      -> Action Q
    SDL.KeycodeR      -> Action R
    SDL.KeycodeS      -> Action S
    SDL.KeycodeT      -> Action T
    SDL.KeycodeU      -> Action U
    SDL.KeycodeV      -> Action V
    SDL.KeycodeW      -> Action W
    SDL.KeycodeX      -> Action X
    SDL.KeycodeY      -> Action Y
    SDL.KeycodeZ      -> Action Z
    _                 -> Action Help

-- | mkIntents handles multiple events
-- <https://github.com/haskell-game/sdl2/issues/241>
mkIntents :: [SDL.Event] -> [Intent]
mkIntents = foldr (\x -> (:) (actionIntent . extractPayload $ x)) [Idle]
