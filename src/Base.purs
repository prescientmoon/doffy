-- | Definitions for the core of the geometry package
module Geometry.Base where

import Prelude

import Control.Alt ((<|>))
import Control.Ask (class Ask, ask)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldr)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Lens (Lens', _Just, over, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem as Opt
import Data.Undefined.NoProblem.Closed as Closed
import Doffy.AABB (AABBLike, AABB)
import Doffy.AABB as AABB
import Doffy.Exists (mapExists)
import Doffy.Forest (Forest)
import Doffy.Forest as Forest
import Doffy.MouseButton (MouseButtons)
import Doffy.Transform (TransformMatrix, inverse, multiplyVector)
import Doffy.Transform as Transform
import Doffy.Utility (Id)
import Doffy.Vector (Vec2, distanceSquared, dotProduct, multiplyScalar, vec2)
import Geometry.TextBaseline (TextBaseline)
import Graphics.Canvas (Context2D)
import Math (pow)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.UIEvent.MouseEvent (MouseEvent)

---------- Types
type EventHandler payload action = (payload -> action)

-- | Event data carried by mouse events on doffy geometries
type CanvasMouseEvent =
  { buttons :: MouseButtons
  , localPosition :: Vec2
  , worldPosition :: Vec2
  , raw :: MouseEvent
  }

-- | The existing type in the canvas library lacks certain props
type TextMetrics =
  { width :: Number
  , fontBoundingBoxAscent :: Number
  }

newtype MapActionF id to from = MapActionF
  { target :: Geometry id from
  , map :: from -> to
  }

-- | A geometry represents anything the library can render to the screen.
-- | The first type parameter represents the id we can identify important points in the trees by
-- | The second type parameters represents the actions propagated on events
data Geometry :: Type -> Type -> Type
data Geometry id action
  = Circle (Record (CircleAttributes id action + GeometryAttributes id action ()))
  | Rect (Record (RectAttributes id action + GeometryAttributes id action ()))
  | Line (Record (LineAttributes id action + GeometryAttributes id action ()))
  | Group (Record (GroupAttributes id action + GeometryAttributes id action ()))
  | Text (Record (TextAttributes id action + GeometryAttributes id action ()))
  | Transform (Record (TransformAttributes id action + GeometryAttributes id action ()))
  -- | Useful when the bounds used for layouting need to be diffrent from the bounds used for rendering.
  | LockBounds (Record (LockBoundsAttributes id action ()))
  -- | Useful for scoping the action type
  | MapAction (Exists (MapActionF id action))
  -- | Useful when metadata about the geometry tree needs to be queried
  | Reporter (Record (ReporterAttributes Id id action ()))
  | None

type Attributes = Type -> Type -> Row Type -> Row Type

type UnknownActionGeometry id = Exists (Geometry id)

-- | Indexed metrics of the data a reporter reported
type ReporterOutput id =
  { absoluteBounds :: HashMap id AABB
  , relativeBounds :: HashMap id AABB
  , transforms :: HashMap id TransformMatrix
  , geometries :: HashMap id (UnknownActionGeometry id)
  , ids :: Forest id
  }

---------- Attributes most shapes contain
type EventAttributes :: Attributes
type EventAttributes id action r =
  ( onClick :: Opt (EventHandler CanvasMouseEvent action)
  , onMousedown :: Opt (EventHandler CanvasMouseEvent action)
  , onMouseup :: Opt (EventHandler CanvasMouseEvent action)
  , onMouseMove :: Opt (EventHandler CanvasMouseEvent action)
  | r
  )

type GeometryAttributes :: Attributes
type GeometryAttributes id action r =
  ( fill :: Opt String
  , stroke :: Opt String
  , weight :: Opt Number
  , alpha :: Opt Number

  -- Here for debugging only (rn)
  , label :: Opt String
  | EventAttributes id action r
  )

type LayeredGeometry id action = Int /\ Geometry id action
type MultiStepRenderer id action = LayeredGeometry id action /\ Array (ReporterOutput id -> LayeredGeometry id action)

---------- Attribute types for individual shapes
type RectAttributes :: Attributes
type RectAttributes id a r = AABBLike r

type CircleAttributes :: Attributes
type CircleAttributes id a r = (position :: Vec2, radius :: Number | r)

type GroupAttributes :: Attributes
type GroupAttributes id action r = (children :: Array (Geometry id action) | r)

type TextAttributes :: Attributes
type TextAttributes id a r =
  ( position :: Vec2
  , text :: String
  , baseline :: Opt TextBaseline
  , font :: Opt String
  | r
  )

type TransformAttributes :: Attributes
type TransformAttributes id action r =
  ( transform :: TransformMatrix
  , target :: Geometry id action
  | r
  )

type LockBoundsAttributes :: Attributes
type LockBoundsAttributes id action r =
  ( bounds :: Lazy (Maybe AABB)
  , target :: Geometry id action
  | r
  )

type ReporterAttributes :: (Type -> Type) -> Attributes
type ReporterAttributes f id action r =
  ( id :: id
  , target :: Geometry id action
  , reportAbsoluteBounds :: f Boolean
  , reportRelativeBounds :: f Boolean
  , reportTransform :: f Boolean
  , reportGeometry :: f Boolean
  | r
  )

type LineAttributes :: Attributes
type LineAttributes id action r =
  ( from :: Vec2
  , to :: Vec2
  , dash :: Opt (Array Number)
  , dashOffset :: Opt Number
  | r
  )

---------- Constructor types
type GeometryConstructor extra =
  forall given action id
   . Closed.Coerce given (Record (extra id action + GeometryAttributes id action ()))
  => given
  -> Geometry id action

---------- Constructors
rect :: GeometryConstructor RectAttributes
rect = Closed.coerce >>> Rect

circle :: GeometryConstructor CircleAttributes
circle = Closed.coerce >>> Circle

-- | Group multiple geometries together
group :: GeometryConstructor GroupAttributes
group = Closed.coerce >>> go
  where
  go attributes@{ children }
    | Array.null children = None
    | otherwise = Group attributes

-- | Apply a transform matrix to a geometry
transform :: GeometryConstructor TransformAttributes
transform = Closed.coerce >>> Transform

-- | Locks the bounds of the children geometry and then applies a function over it.
-- | Useful for applying some offset without changing the layout.
lockBounds :: forall id action. Ask Context2D => (Geometry id action -> Geometry id action) -> Geometry id action -> Geometry id action
lockBounds f geometry = LockBounds { target: f geometry, bounds: Lazy.defer \_ -> bounds geometry }

text :: GeometryConstructor TextAttributes
text = Closed.coerce >>> Text

line :: GeometryConstructor LineAttributes
line = Closed.coerce >>> Line

-- | Scope the action of a geometry.
-- | Ex: 
-- | ```purescript
-- | foo :: Geometry Foo A
-- | foo = ...
-- | 
-- | aToB :: A -> B
-- | aToB = ...
-- |
-- | bar :: Geometry Foo B
-- | bar = mapAction aToB foo 
-- | ````
mapAction :: forall id from to. (from -> to) -> Geometry id from -> Geometry id to
mapAction map target = { target, map } # MapActionF # mkExists # MapAction

-- | A reporter is a component which caches metadata like:
-- |  - the absolute positions of nodes
-- |  - the relative positions of nodes
-- |  - local transform matrices
-- |  - actual geometry 
-- | for later use.
reporter :: forall given action id. Closed.Coerce given (Record (ReporterAttributes Opt id action ())) => given -> Geometry id action
reporter = Closed.coerce >>> withDefaults >>> Reporter
  where
  withDefaults :: Record (ReporterAttributes Opt id action ()) -> _
  withDefaults incomplete =
    { target: incomplete.target
    , id: incomplete.id
    , reportAbsoluteBounds: Opt.fromOpt false incomplete.reportAbsoluteBounds
    , reportRelativeBounds: Opt.fromOpt false incomplete.reportRelativeBounds
    , reportTransform: Opt.fromOpt false incomplete.reportTransform
    , reportGeometry: Opt.fromOpt false incomplete.reportGeometry
    }

-- | Annotates and reports the absolute bounds for a given geometry
annotate :: forall id action. id -> Geometry id action -> Geometry id action
annotate id target = reporter { id, target, reportAbsoluteBounds: true }

---------- Helpers
translate :: forall id action. Vec2 -> Geometry id action -> Geometry id action
translate amount (Circle attributes) =
  Circle $ over _position ((+) amount) attributes
translate amount (Rect attributes) =
  Rect $ over _position ((+) amount) attributes
translate amount (Text attributes) =
  Text $ over _position ((+) amount) attributes
translate amount (Line attributes) = Line attributes
  { from = amount + attributes.from
  , to = amount + attributes.to
  }
translate amount (Group attributes) =
  Group $ over (_children <<< traversed) (translate amount) attributes
translate amount (Reporter attributes) =
  Reporter $ over _target (translate amount) attributes
translate amount (Transform attributes) =
  Transform $ over _transform (_ <> Transform.translate amount) attributes
translate amount (LockBounds attributes) = LockBounds
  { target: translate amount attributes.target
  , bounds: attributes.bounds <#> over (_Just <<< _position) ((+) amount)
  }
translate amount (MapAction existential) = MapAction $ mapExists mapInner existential
  where
  mapInner :: MapActionF id action ~> MapActionF id action
  mapInner = over (_Newtype <<< _target) $ translate amount
translate amount None = None

-- | Calculate the minimum rectangle needed to surround the shape
bounds :: forall id action. Ask Context2D => Geometry id action -> Maybe AABB
bounds (Circle attributes) = Just
  { position: attributes.position - radius2
  , size: ((*) 2.0) <$> radius2
  }
  where
  radius2 = vec2 attributes.radius attributes.radius
bounds (Rect { position, size }) = Just { position, size }
bounds (Group { children }) = foldr merger Nothing $ bounds <$> children
  where
  merger = case _, _ of
    Just a, Just b -> Just $ AABB.union a b
    a, b -> a <|> b
bounds (LockBounds attributes) = Lazy.force attributes.bounds
bounds (Transform attributes) = do
  innerBounds <- bounds attributes.target
  AABB.fromPoints $ points $ Transform attributes
bounds (Text attributes) = Just do
  let metrics = measureText ask attributes.font attributes.text
  { position: attributes.position
  , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
  }
bounds (MapAction inner) = inner # runExists (unwrap >>> _.target >>> bounds)
bounds (Reporter { target }) = bounds target
bounds (Line { from, to }) = AABB.fromPoints [ from, to ]
bounds None = Nothing

-- | Returns a polygon surrounding the shape
points :: forall id action. Ask Context2D => Geometry id action -> Array Vec2
points (Transform { transform, target }) = points target <#> multiplyVector transform
points (Reporter { target }) = points target
points (Line { from, to }) = [ from, to ]
points a = bounds a # maybe [] (AABB.points >>> NonEmptyArray.toArray)

pointInside :: forall id action. Ask Context2D => Vec2 -> Geometry id action -> Boolean
pointInside point (Circle attributes) =
  distanceSquared point attributes.position < attributes.radius `pow` 2.0
pointInside point (Group { children }) = Array.any (pointInside point) children
pointInside point (LockBounds { target }) = pointInside point target
pointInside point shape@(Transform { target }) = pointInside projected target
  where
  projected = toLocalCoordinates shape point
pointInside point (Line { from, to, weight }) = distanceSquared referencePoint point <= actualWeight `pow` 2.0
  where
  referencePoint
    | length <= 0.0 = from
    | otherwise = do
        let relative = to - from
        let product = clamp 0.0 1.0 $ dotProduct (point - from) relative / length
        from + relative `multiplyScalar` product
  length = distanceSquared from to
  actualWeight = Opt.fromOpt 1.0 weight
pointInside point shape = bounds shape # maybe false (AABB.pointInside point)

-- TODO: find a way to cache the inverse
toLocalCoordinates :: forall id action. Geometry id action -> Vec2 -> Vec2
toLocalCoordinates (Transform { target, transform }) point = multiplyVector (inverse transform) point
toLocalCoordinates _ point = point

-- | Get an array with all the children owned by a geometry
children :: forall id action. Geometry id action -> forall result. (forall subaction. (subaction -> action) -> Geometry id subaction -> result) -> Array result
children (Group { children }) f = f identity <$> children
children (Reporter { target }) f = [ f identity target ]
children (Transform { target }) f = [ f identity target ]
children (LockBounds { target }) f = [ f identity target ]
children (MapAction existential) f = [ existential # runExists \(MapActionF { target, map }) -> f map target ]
children _ _ = []

-- | Get an existential with the attributes carried around by a geometry
attributes :: forall id action result. Geometry id action -> result -> (forall r. Record (GeometryAttributes id action r) -> result) -> result
attributes (Circle attributes) _ f = f attributes
attributes (Rect attributes) _ f = f attributes
attributes (Line attributes) _ f = f attributes
attributes (Group attributes) _ f = f attributes
attributes (Text attributes) _ f = f attributes
attributes (Transform attributes) _ f = f attributes
attributes None _ f = f $ (Closed.coerce {} :: Record (GeometryAttributes id action ()))
attributes _ default _ = default

-- | Collect analytics reported by all the reporter components inside a geometry
report :: forall id action. Hashable id => Ask Context2D => Geometry id action -> ReporterOutput id
report (Rect _) = emptyReporterOutput
report (Circle _) = emptyReporterOutput
report (Line _) = emptyReporterOutput
report (Text _) = emptyReporterOutput
report None = emptyReporterOutput
report (MapAction existential) = existential # runExists \(MapActionF { target }) -> report target
report (LockBounds { target }) = report target
report (Group attributes)
  = attributes.children <#> report
  # foldr mergeReporterOutputs emptyReporterOutput
report (Transform attributes)
  = over _absoluteBounds (map transformBounds)
  $ over _transforms (map transformTransform)
  $ report attributes.target
  where
  transformBounds :: AABB -> AABB
  transformBounds = AABB.points >>> map (multiplyVector attributes.transform) >>> AABB.fromPoints1

  transformTransform :: TransformMatrix -> TransformMatrix
  transformTransform = (<>) attributes.transform
report (Reporter { target, id, reportAbsoluteBounds, reportRelativeBounds, reportTransform, reportGeometry }) =
  case bounds target of
    Nothing -> childReport { ids = ids }
    Just bounds ->
      { absoluteBounds:
          childReport.absoluteBounds # applyWhen reportAbsoluteBounds (HashMap.insert id bounds)
      , relativeBounds:
          childReport.relativeBounds # applyWhen reportRelativeBounds (HashMap.insert id bounds)
      , transforms: childReport.transforms # applyWhen reportTransform (HashMap.insert id Transform.identityMatrix)
      , geometries: childReport.geometries # applyWhen reportGeometry (HashMap.insert id $ mkExists target)
      , ids
      }
  where
  applyWhen :: forall a. Boolean -> (a -> a) -> a -> a
  applyWhen condition f input
    | condition = f input
    | otherwise = input

  ids :: Forest id
  ids = Forest.annotate id childReport.ids

  childReport :: ReporterOutput id
  childReport = report target

mergeReporterOutputs :: forall id. Hashable id => ReporterOutput id -> ReporterOutput id -> ReporterOutput id
mergeReporterOutputs a b =
  { absoluteBounds: a.absoluteBounds `HashMap.union` b.absoluteBounds
  , relativeBounds: a.relativeBounds `HashMap.union` b.relativeBounds
  , transforms: a.transforms `HashMap.union` b.transforms
  , geometries: a.geometries `HashMap.union` b.geometries
  , ids: a.ids <|> b.ids
  }

---------- Constants
emptyReporterOutput :: forall id. ReporterOutput id
emptyReporterOutput =
  { absoluteBounds: HashMap.empty
  , relativeBounds: HashMap.empty
  , transforms: HashMap.empty
  , geometries: HashMap.empty
  , ids: empty
  }

---------- Lenses
_position :: forall r. Lens' { position :: Vec2 | r } Vec2
_position = prop (Proxy :: _ "position")

_children :: forall id action r. Lens' { children :: Array (Geometry id action) | r } (Array (Geometry id action))
_children = prop (Proxy :: _ "children")

_transform :: forall r. Lens' { transform :: TransformMatrix | r } TransformMatrix
_transform = prop (Proxy :: _ "transform")

_target :: forall id action r. Lens' { target :: Geometry id action | r } (Geometry id action)
_target = prop (Proxy :: _ "target")

_absoluteBounds :: forall id. Lens' (ReporterOutput id) (HashMap id AABB)
_absoluteBounds = prop (Proxy :: _ "absoluteBounds")

_transforms :: forall id. Lens' (ReporterOutput id) (HashMap id TransformMatrix)
_transforms = prop (Proxy :: _ "transforms")

---------- Foreign imports
foreign import measureText :: Context2D -> Opt String -> String -> TextMetrics

---------- Typeclass instances
derive instance Newtype (MapActionF id to from) _
