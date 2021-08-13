module Doffy.Utility where

import Data.Tuple.Nested (type (/\))

type Id :: forall k. k -> k
type Id a = a

type Const :: forall k1 k2. k1 -> k2 -> k1
type Const a b = a

type Pair a = a /\ a