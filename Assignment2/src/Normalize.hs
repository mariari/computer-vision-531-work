module Normalize
  (normalizeValues,
   Normalize(Zeroc,Same)) where

import Misc
-- Really only going to use ZeroC for now, but can expand later
data Normalize = Zeroc
               | Same


normalizeValues :: (Functor f1, Fractional b, Foldable f1, Ord b) => Normalize -> f1 [b] -> f1 [b]
normalizeValues Same  xss = undefined
normalizeValues Zeroc xss = (/ greatest) <$$> xss
  where flattened = concat xss
        greatest  = maximum (abs <$> flattened)