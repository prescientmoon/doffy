module Doffy.Exists where

import Prelude
import Data.Exists (Exists, mkExists, runExists)

-- | Map the interoior of an existential
mapExists :: forall f. (forall a. f a -> f a) -> Exists f -> Exists f
mapExists f = runExists (f >>> mkExists)
