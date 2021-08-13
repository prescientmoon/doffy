-- | Cofree based forests
module Doffy.Forest where

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Maybe (Maybe)
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZipperArray

---------- Types
-- | A single tree node contains a label and a tree of children
type Tree a = Cofree Array a

-- | Similar to Cofree, except not lazy, and can be empty
type Forest a = Array (Tree a)

type ForestZipper a = ZipperArray (Tree a)

---------- Helpers
-- | Construct a forest containing no trees
emptyForest :: forall a. a -> Forest a
emptyForest label = [ mkCofree label [] ]

-- | Add a label onto a forest
annotate :: forall a. a -> Forest a -> Forest a
annotate label forest = [ mkCofree label forest ]

-- | Create a zipper array out of the trees at the top level of a forest
toZipper :: forall a. Forest a -> Maybe (ForestZipper a)
toZipper = ZipperArray.fromArray