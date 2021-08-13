module Doffy.MouseButton
  ( MouseButtons
  , MouseButton
  , isPressed
  , nothingPressed
  , leftButton
  , rightButton
  ) where

import Prelude
import Data.Int.Bits (and)

-- | A set of pressed mouse buttons
newtype MouseButtons = MouseButtons Int

-- | A set of pressed mouse buttons containing a single button
type MouseButton = MouseButtons

-- | Check if no buttons are pressed
nothingPressed :: MouseButtons -> Boolean
nothingPressed (MouseButtons bits) = bits == 0

-- | Check if a button is inside a set of buttons
isPressed :: MouseButtons -> MouseButton -> Boolean
isPressed (MouseButtons bits) (MouseButtons button) = 0 /= (bits `and` button)

-- | The left button on the mouse
leftButton :: MouseButton
leftButton = MouseButtons 1

-- | The right button on the mouse
rightButton :: MouseButton
rightButton = MouseButtons 2
