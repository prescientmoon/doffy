module Doffy.Vector
  ( Vec2
  , Axis(..)
  , vec2
  , x
  , y
  , toTuple
  , fromTuple
  , distance
  , distanceSquared
  , other
  , indexByAxis
  , mapAxis
  , lmapAxis
  , rmapAxis
  , bimapAxis
  , buildFromAxis
  , greaterThan
  , smallerThan
  , multiplyScalar
  , dotProduct
  , _ixVector
  , _x
  , _y
  , _axis
  , _otherAxis
  , _vectorToTuple
  ) where

import Prelude

import Data.Lens (Iso', Lens', iso, lens, over)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, D2, d0, d1)
import Data.Vec (Vec, (!!))
import Data.Vec as Vec

---------- Types
-- | A vector 2 is a pair of an x coordinate and a y coordinate
type Vec2 = Vec D2 Number

-- | An axis can be used to index over a Vec2
data Axis = X | Y

---------- Helpers
-- | Get the x component of the vector
x :: Vec2 -> Number
x vec = vec !! d0

-- | Get the y component of the vector
y :: Vec2 -> Number
y vec = vec !! d1

-- | Construct a vector from its x and y components
vec2 :: Number -> Number -> Vec2
vec2 x y = Vec.vec2 x y

-- | Convert a vector to a tuple
toTuple :: Vec2 -> Number /\ Number
toTuple vec = (x vec) /\ (y vec)

-- | Conert a tuple to a vector
fromTuple :: Number /\ Number -> Vec2
fromTuple = uncurry vec2

-- | Compute wether both components of a vector are smaller than another's
smallerThan :: Vec2 -> Vec2 -> Boolean
smallerThan a b = x a < x b && y a < y b

-- | Compute wether both components of a vector are greater than another's
greaterThan :: Vec2 -> Vec2 -> Boolean
greaterThan a b = x a > x b && y a > y b

-- | Invert an axis
other :: Axis -> Axis
other X = Y
other Y = X

-- | Use an axis to index a vector
indexByAxis :: Axis -> Vec2 -> Number
indexByAxis X = x
indexByAxis Y = y

-- | Build a vector from 2 numbers where the first is the component on an arbitrary axis
buildFromAxis :: Axis -> Number -> Number -> Vec2
buildFromAxis X a b = vec2 a b
buildFromAxis Y a b = vec2 b a

-- | Update the coordinate on a certain axis by running a function over it
mapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
mapAxis axis = over (_axis axis)

-- | Update the coordinate on a certain axis by running a function over it
lmapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
lmapAxis = mapAxis

-- | Update the coordinate on the inverse of a certain axis by running a function over it
rmapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
rmapAxis = other >>> mapAxis

-- | Run a function over axis, where the first (function) is run over the provided axis
bimapAxis :: Axis -> (Number -> Number) -> (Number -> Number) -> Vec2 -> Vec2
bimapAxis axis f g = over (_axis axis) f >>> over (_otherAxis axis) g

---------- Lenses
-- | Sized vector equivalent of `ix`
_ixVector :: forall a s i. Nat i => Lt i s => i -> Lens' (Vec s a) a
_ixVector index = lens get set
  where
  get vec = vec !! index
  set vec newX = Vec.updateAt index newX vec

-- | Focus on the x component of the vector
_x :: Lens' Vec2 Number
_x = _ixVector d0

-- | Focus on the y component of the vector
_y :: Lens' Vec2 Number
_y = _ixVector d1

-- | Focus on one of the axis of the vector
_axis :: Axis -> Lens' Vec2 Number
_axis X = _x
_axis Y = _y

-- | Focus on the iverse of one of the axis of the vector
_otherAxis :: Axis -> Lens' Vec2 Number
_otherAxis axis = _axis (other axis)

-- | Cast between a vector and a tuple
_vectorToTuple :: Iso' Vec2 (Number /\ Number)
_vectorToTuple = iso toTuple fromTuple

---------- Foreign stuff
foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number
foreign import dotProduct :: Vec2 -> Vec2 -> Number
foreign import multiplyScalar :: Vec2 -> Number -> Vec2