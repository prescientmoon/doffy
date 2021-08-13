-- | Types and helpers for working with text baselines
module Geometry.TextBaseline
  ( TextBaseline
  , top
  , hanging
  , middle
  , alphabetic
  , ideographic
  , bottom
  ) where

---------- Types
-- | The text baseline specifies where the y coordinate stands relative to the text
newtype TextBaseline = TextBaseline String

---------- Constants
top :: TextBaseline
top = TextBaseline "top"

hanging :: TextBaseline
hanging = TextBaseline "hanging"

middle :: TextBaseline
middle = TextBaseline "middle"

alphabetic :: TextBaseline
alphabetic = TextBaseline "alphabetic"

ideographic :: TextBaseline
ideographic = TextBaseline "ideographic"

bottom :: TextBaseline
bottom = TextBaseline "bottom"
