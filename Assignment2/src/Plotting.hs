module Plotting
  (plotDCT
  ) where

import Graphics.Rendering.Plot.HMatrix
import Numeric.LinearAlgebra.Data
import Cosine
import Normalize
import Data.Monoid
import Misc

-- the fmap ((-1) :) . (<> [-1]) is for padding the top and bottom
-- this pads the left and right (padList <>) . (<> padList)
plotDCT :: Int -> IO ()
plotDCT n = imshow (fromBlocks (fromLists <$$> padded))
  where padded     = fmap (((-1) :) . (<> [-1])) . (padList <>) . (<> padList) <$$> normalized
        padList    = [replicate n (-1)]
        normalized = normalizeValues Zeroc <$$> nbyN n