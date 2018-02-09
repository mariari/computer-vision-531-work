module MatrixHelper (linearFilter, (⊕)) where

import           Data.Matrix  as M
import           Data.Monoid
-- TestImage data =====================================================================
matrix310 :: (Num a, Enum a) => Matrix a
matrix310 = fromLists [[45,60,98,127,132,133,137,133]
                      ,[46,65,98,123,126,128,131,133]
                      ,[47,65,96,115,119,123,135,137]
                      ,[47,63,91,107,113,122,138,134]]

fromBookOffset = fromList 3 3 $ replicate 4 0.1 <> (0.2 : repeat 0.1)

-- Functions ======================================================================
extractWindows :: Int -> Int -> Matrix a -> Matrix (Matrix a)
extractWindows row col mat = matrix (nrows mat - row + 1) (ncols mat - col + 1) f
  where
    f (i,j) = submatrix i (i + row - 1) j (j + col - 1) mat

linearFilter :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
linearFilter filt = fmap (round . foldr (+) 0 . elementwise (*) filt) . extractWindows row col
  where row = nrows filt
        col = ncols filt

(⊕) :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
(⊕) = flip linearFilter