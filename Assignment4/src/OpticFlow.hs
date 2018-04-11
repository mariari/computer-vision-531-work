module OpticFlow
  ( δx
  , δy
  , δt
  ) where

import Filters
import Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2

infin :: Fractional a => a
infin = 1/0

-- n is the size of the window
-- m is the size of the local area
convfn :: (Source r b, Num b) => Int -> Int -> Array r DIM2 b -> Array r DIM2 b -> Array D DIM2 b
convfn n m img1 img2 = R.fromFunction newSize f
  where
    sideSize        = n `div` 2
    Z :. i :. j     = extent img1
    offset x y      = ix2 (x - sideSize) (y - sideSize)
    newSize         = offset i j
    extractNbyN x y = extract (offset x y) (ix2 n n)
    f (Z :. x :. y) = undefined


data WindowSize = Window3
                | Window5
                | Window7

δx :: (Num b, Source r b) => WindowSize -> Array r DIM2 b -> Array PC5 DIM2 b
δx Window3 = sobelX
δx Window5 = sobelX5
δx Window7 = sobelX7

δy :: (Num b, Source r b) => WindowSize -> Array r DIM2 b -> Array PC5 DIM2 b
δy Window3 = sobelY
δy Window5 = sobelY5
δy Window7 = sobelY7

δt :: (Shape sh, Source r1 c, Source r2 c, Num c) => Array r1 sh c -> Array r2 sh c -> Array D sh c
δt = (-^)


