module Doffy.AABB where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Lens (Iso, iso)
import Data.Maybe (Maybe)
import Data.Vec as Vec
import Doffy.Vector (Vec2, greaterThan, smallerThan, vec2, x, y)
import Graphics.Canvas (Rectangle)

---------- Types
type AABBLike r = (position :: Vec2, size :: Vec2 | r)
type AABB = Record (AABBLike ())

type PointForm = { min :: Vec2, max :: Vec2 }

---------- Helpers
fromMinMax :: PointForm -> AABB
fromMinMax points =
  { position: points.min
  , size: points.max - points.min
  }

toMinMax :: forall r. Record (AABBLike r) -> PointForm
toMinMax aabb =
  { min: aabb.position
  , max: aabb.position + aabb.size
  }

union :: AABB -> AABB -> AABB
union a b = fromMinMax
  { min: Vec.zipWith min a.position b.position
  , max: Vec.zipWith max (toMinMax a).max (toMinMax b).max
  }

-- | Calculate the center of some bounding box
center :: AABB -> Vec2
center { position, size } = position + map (_ / 2.0) size

-- | Calculate wether a point is inside a rect
pointInside :: Vec2 -> AABB -> Boolean
pointInside point { position, size } = point `greaterThan` position && point `smallerThan` (position + size)

-- | Calculates the bounds of a polygon. Returns Nothing if the point array is empty
fromPoints :: Array Vec2 -> Maybe AABB
fromPoints points = ado
  points <- NonEmptyArray.fromArray points
  in fromPoints1 points

-- | Calculate the bounds of a polygon guaranteed to have at least one point.
fromPoints1 :: NonEmptyArray Vec2 -> AABB
fromPoints1 points = fromMinMax { min: minPoint, max: maxPoint }
  where
  minPoint = NonEmptyArray.foldr1 (Vec.zipWith min) points
  maxPoint = NonEmptyArray.foldr1 (Vec.zipWith max) points

-- | Get an array with all 4 points forming the aabb shape
points :: forall r. Record (AABBLike r) -> NonEmptyArray Vec2
points { position, size } =
  NonEmptyArray.cons' position
    [ vec2 (x position + x size) (y position)
    , position + size
    , vec2 (x position) (y position + y size)
    ]

-- | Convert to a rectangle renderable on the canvas
toCanvasRect :: forall r. Record (AABBLike r) -> Rectangle
toCanvasRect { position, size } =
  { x: x position
  , y: y position
  , width: x size
  , height: y size
  }

---------- Lenses
_AABBToPointForm :: forall r. Iso (Record (AABBLike r)) AABB PointForm PointForm
_AABBToPointForm = iso toMinMax fromMinMax

_AABBToPoints :: forall r. Iso (Record (AABBLike r)) AABB (NonEmptyArray Vec2) (NonEmptyArray Vec2)
_AABBToPoints = iso points fromPoints1