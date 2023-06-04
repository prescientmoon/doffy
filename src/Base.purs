-- | Definitions for the core of the geometry package
module Geometry.Base where

import Prelude

import Control.Alt ((<|>))
import Control.Ask (class Ask, ask)
import Control.Plus (empty)
import Data.Array as Array
import Data.Exists (Exists)
import Data.Foldable (foldr)
import Data.Functor.Mu (Mu(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem as Opt
import Data.Undefined.NoProblem.Closed as Closed
import Doffy.AABB (AABBLike, AABB)
import Doffy.AABB as AABB
import Doffy.Forest (Forest)
import Doffy.MouseButton (MouseButtons)
import Doffy.Transform (TransformMatrix, inverse, multiplyVector)
import Doffy.Transform as Transform
import Doffy.Utility (Id)
import Doffy.Vector (Vec2, distanceSquared, dotProduct, multiplyScalar, vec2)
import Geometry.TextBaseline (TextBaseline)
import Graphics.Canvas (Context2D)
import Math (pow)
import Matryoshka (Algebra, cata)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
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

data TagFilter a
  = Whitelist (HashSet a)
  | Blacklist (HashSet a)

-- | The underlying data type used to represent geometries. Usually wrapped in `Mu`
data GeometryF id action recurse
  = Circle (Record (CircleAttributes id action recurse + GeometryAttributes id action recurse ()))
  | Rect (Record (RectAttributes id action recurse + GeometryAttributes id action recurse ()))
  | Line (Record (LineAttributes id action recurse + GeometryAttributes id action recurse ()))
  | Text (Record (TextAttributes id action recurse + GeometryAttributes id action recurse ()))
  | None

  | Group (Record (GroupAttributes id action recurse + GeometryAttributes id action recurse ()))
  | Transform (Record (TransformAttributes id action recurse + GeometryAttributes id action recurse ()))
  -- | Useful when metadata about the geometry tree needs to be queried
  | Reporter (Record (ReporterAttributes Id id action recurse ()))

-- | A geometry represents anything the library can render to the screen.
-- | The first type parameter represents the id we can identify important points in the trees by
-- | The second type parameters represents the actions propagated on events
type Geometry id action = Mu (GeometryF id action)

type Attributes = Type -> Type -> Type -> Row Type -> Row Type

newtype UnknownActionGeometryF id action = UnknownActionGeometry (Geometry id action)
type UnknownActionGeometry id = Exists (UnknownActionGeometryF id)

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
type EventAttributes id action recurse remaining =
  ( onClick :: Opt (EventHandler CanvasMouseEvent action)
  , onMousedown :: Opt (EventHandler CanvasMouseEvent action)
  , onMouseup :: Opt (EventHandler CanvasMouseEvent action)
  , onMouseMove :: Opt (EventHandler CanvasMouseEvent action)
  | remaining
  )

type GeometryAttributes :: Attributes
type GeometryAttributes id action recurse remaining =
  ( fill :: Opt String
  , stroke :: Opt String
  , weight :: Opt Number
  , alpha :: Opt Number
  , tags :: Opt (Array String)

  -- Here for debugging only (rn)
  , label :: Opt String
  | EventAttributes id action recurse remaining
  )

type LayeredGeometry id action = Int /\ Geometry id action
type MultiStepRenderer id action = LayeredGeometry id action /\ Array (ReporterOutput id -> LayeredGeometry id action)

---------- Attribute types for individual shapes
type RectAttributes :: Attributes
type RectAttributes id action recurse remaining = AABBLike remaining

type CircleAttributes :: Attributes
type CircleAttributes id action recurse remaining = (position :: Vec2, radius :: Number | remaining)

type GroupAttributes :: Attributes
type GroupAttributes id action recurse remaining = (children :: Array recurse | remaining)

type TextAttributes :: Attributes
type TextAttributes id action recurse remaining =
  ( position :: Vec2
  , text :: String
  , baseline :: Opt TextBaseline
  , font :: Opt String
  | remaining
  )

type TransformAttributes :: Attributes
type TransformAttributes id action recurse remaining =
  ( transform :: TransformMatrix
  , target :: recurse
  | remaining
  )

type ReporterAttributes :: (Type -> Type) -> Attributes
type ReporterAttributes f id action recurse remaining =
  ( id :: id
  , target :: recurse
  , reportAbsoluteBounds :: f Boolean
  , reportRelativeBounds :: f Boolean
  , reportTransform :: f Boolean
  , reportGeometry :: f Boolean
  | remaining
  )

type LineAttributes :: Attributes
type LineAttributes id action recurse remaining =
  ( from :: Vec2
  , to :: Vec2
  , dash :: Opt (Array Number)
  , dashOffset :: Opt Number
  | remaining
  )

---------- Constructor types
type GeometryConstructor :: Attributes -> Type
type GeometryConstructor extra =
  forall given action id
   . Closed.Coerce given (Record (extra id action (Geometry id action) + GeometryAttributes id action (Geometry id action) ()))
  => given
  -> Geometry id action

---------- Constructors
rect :: GeometryConstructor RectAttributes
rect = Closed.coerce >>> Rect >>> In

circle :: GeometryConstructor CircleAttributes
circle = Closed.coerce >>> Circle >>> In

-- | Group multiple geometries together
group :: GeometryConstructor GroupAttributes
group = Closed.coerce >>> go >>> In
  where
  go attributes@{ children }
    | Array.null children = None
    | otherwise = Group attributes

-- | Apply a transform matrix to a geometry
transform :: GeometryConstructor TransformAttributes
transform = Closed.coerce >>> Transform >>> In

text :: GeometryConstructor TextAttributes
text = Closed.coerce >>> Text >>> In

line :: GeometryConstructor LineAttributes
line = Closed.coerce >>> Line >>> In

-- | A reporter is a component which caches metadata like:
-- |  - the absolute positions of nodes
-- |  - the relative positions of nodes
-- |  - local transform matrices
-- |  - actual geometry 
-- | for later use.
reporter
  :: forall given action id
   . Closed.Coerce given (Record (ReporterAttributes Opt id action (Geometry id action) ()))
  => given
  -> Geometry id action
reporter = Closed.coerce >>> withDefaults >>> Reporter >>> In
  where
  withDefaults :: Record (ReporterAttributes Opt id action _ ()) -> _
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
coerceGeometry :: forall id action from to. GeometryF id action from -> Maybe (GeometryF id action to)
coerceGeometry (Reporter _) = Nothing
coerceGeometry (Transform _) = Nothing
coerceGeometry (Group _) = Nothing
coerceGeometry a = Just $ unsafeCoerce a

translate :: forall id action. Vec2 -> Geometry id action -> Geometry id action
translate amount (In None) = In None
translate amount (In (Transform attributes)) = In
  $ Transform
  $ over _transform (_ <> Transform.translate amount) attributes
translate amount target = transform
  { transform: Transform.translate amount
  , target
  }

filteredBounds :: forall id action. Ask Context2D => TagFilter String -> Algebra (GeometryF id action) (Lazy (Maybe AABB))
filteredBounds filter = algebra
  where
  algebra :: Algebra (GeometryF id action) (Lazy (Maybe AABB))
  algebra geom
    | applyFilter geom = Lazy.defer
        \_ -> boundsAlgebra $ map Lazy.force geom
    | otherwise = pure Nothing

  tagIsAllowed tag = case filter of
    Whitelist set -> HashSet.member tag set
    Blacklist set -> not $ HashSet.member tag set

  tagsAreAllowed = Array.all tagIsAllowed

  applyFilter geometry = attributes geometry true
    \{ tags } -> case Opt.toMaybe tags of
      Just tags -> tagsAreAllowed tags
      _ -> true

bounds :: forall id action. Ask Context2D => Geometry id action -> (Maybe AABB)
bounds = cata boundsAlgebra

-- | Calculate the minimum rectangle needed to surround the shape
boundsAlgebra :: forall id action. Ask Context2D => Algebra (GeometryF id action) (Maybe AABB)
boundsAlgebra (Circle attributes) = Just
  { position: attributes.position - radius2
  , size: ((*) 2.0) <$> radius2
  }
  where
  radius2 = vec2 attributes.radius attributes.radius
boundsAlgebra (Rect { position, size }) = Just { position, size }
boundsAlgebra (Group { children }) = foldr merger Nothing children
  where
  merger = case _, _ of
    Just a, Just b -> Just $ AABB.union a b
    a, b -> a <|> b
boundsAlgebra (Transform attributes) = ado
  innerBounds <- attributes.target
  in
    AABB.points innerBounds
      # map (multiplyVector attributes.transform)
      # AABB.fromPoints1
boundsAlgebra (Text attributes) = Just do
  let metrics = measureText ask attributes.font attributes.text
  { position: attributes.position
  , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
  }
boundsAlgebra (Reporter { target }) = target
boundsAlgebra (Line { from, to }) = AABB.fromPoints [ from, to ]
boundsAlgebra None = Nothing

pointInside :: forall id action. Ask Context2D => Algebra (GeometryF id action) (Vec2 -> Boolean)
pointInside (Circle attributes) point =
  distanceSquared point attributes.position < attributes.radius `pow` 2.0
pointInside (Group { children }) point = Array.any (\child -> child point) children
pointInside shape@(Transform { target }) point = target projected
  where
  projected = toLocalCoordinates shape point
pointInside (Line { from, to, weight }) point = distanceSquared referencePoint point <= actualWeight `pow` 2.0
  where
  referencePoint
    | length <= 0.0 = from
    | otherwise = do
        let relative = to - from
        let product = clamp 0.0 1.0 $ dotProduct (point - from) relative / length
        from + relative `multiplyScalar` product
  length = distanceSquared from to
  actualWeight = Opt.fromOpt 1.0 weight
pointInside shape point = fromMaybe false do
  shape <- coerceGeometry shape
  shapeBounds <- bounds $ In shape
  Just $ AABB.pointInside point shapeBounds

-- TODO: find a way to cache the inverse
toLocalCoordinates :: forall id action child. GeometryF id action child -> Vec2 -> Vec2
toLocalCoordinates (Transform { transform }) point = multiplyVector (inverse transform) point
toLocalCoordinates _ point = point

-- | Get an array with all the children owned by a geometry
children :: forall id action. GeometryF id action ~> Array
children (Group { children }) = children
children (Reporter { target }) = [ target ]
children (Transform { target }) = [ target ]
children _ = []

-- | Get an existential with the attributes carried around by a geometry
attributes :: forall id action inner result. GeometryF id action inner -> result -> (forall r. Record (GeometryAttributes id action inner r) -> result) -> result
attributes (Circle attributes) _ f = f attributes
attributes (Rect attributes) _ f = f attributes
attributes (Line attributes) _ f = f attributes
attributes (Group attributes) _ f = f attributes
attributes (Text attributes) _ f = f attributes
attributes (Transform attributes) _ f = f attributes
attributes None _ f = f $ (Closed.coerce {} :: Record (GeometryAttributes id action inner ()))
attributes _ default _ = default

{- -- | Collect analytics reported by all the reporter components inside a geometry
report :: forall id action. Hashable id => Ask Context2D => Algebra (GeometryF id action) (ReporterOutput id)
report (Rect _) = emptyReporterOutput
report (Circle _) = emptyReporterOutput
report (Line _) = emptyReporterOutput
report (Text _) = emptyReporterOutput
report None = emptyReporterOutput
report (Group attributes)
  = attributes.children
  # foldr mergeReporterOutputs emptyReporterOutput
report (Transform attributes)
  = over _absoluteBounds (map transformBounds)
  $ over _transforms (map transformTransform)
  $ attributes.target
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
  childReport = target -}

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
derive instance Functor (GeometryF id action)