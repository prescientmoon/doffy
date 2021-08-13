module Doffy.Transform
  ( TransformMatrix
  , translate
  , rotate
  , scale
  , scaleXY
  , inverse
  , transform
  , multiplyVector
  , composeMatrices
  , identityMatrix
  , rotateAround
  ) where

import Prelude

import Data.Typelevel.Num (D6)
import Data.Vec (Vec)
import Doffy.Vector (Vec2)

newtype TransformMatrix = TransformMatrix (Vec D6 Number)

---------- Foreign imports
foreign import translate :: Vec2 -> TransformMatrix
foreign import rotate :: Number -> TransformMatrix
foreign import scale :: Number -> TransformMatrix
foreign import scaleXY :: Vec2 -> TransformMatrix
foreign import inverse :: TransformMatrix -> TransformMatrix
foreign import transform :: Vec2 -> Number -> Vec2 -> TransformMatrix
foreign import multiplyVector :: TransformMatrix -> Vec2 -> Vec2
foreign import composeMatrices :: TransformMatrix -> TransformMatrix -> TransformMatrix
foreign import identityMatrix :: TransformMatrix
foreign import rotateAround :: Vec2 -> Number -> TransformMatrix

---------- Typeclass instances
instance Semigroup TransformMatrix where
  append = composeMatrices

instance Monoid TransformMatrix where
  mempty = identityMatrix
