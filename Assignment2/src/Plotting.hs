module Plotting
  (
  ) where

import Graphics.Rendering.Plot.HMatrix
import Numeric.LinearAlgebra.Data
import Cosine
import Normalize
import Data.Monoid
import Misc

-- the fmap ((-1) :) . (<> [-1]) is for padding the top and bottom
-- this pads the left and right (padList <>) . (<> padList)
plotDCT :: IO ()
plotDCT = imshow (fromBlocks (fromLists <$$> padded))
  where padded     = fmap (((-1) :) . (<> [-1])) . (padList <>) . (<> padList) <$$> normalized
        padList    = [replicate 8 (-1)]
        normalized = normalizeValues Zeroc <$$> eightByEight