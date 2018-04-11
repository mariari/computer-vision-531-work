{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Filters (
  sobelEdgeX,
  sobelEdgeY,
  gausianStencilY,
  gausianStencilX,
  sobelX,
  sobelY,
  sobelX5,
  sobelY5,
  sobelX7,
  sobelY7,
  blur
  ) where

import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

sobelEdgeX :: Num a => Stencil DIM2 a
sobelEdgeY :: Num a => Stencil DIM2 a
sobelEdgeX = [stencil2| -1 0 1
                        -2 0 2
                        -1 0 1|]

sobelEdgeY = [stencil2| -1 -2 -1
                         0  0  0
                         1  2  1|]


sobelEdgeX5 :: Num a => Stencil DIM2 a
sobelEdgeY5 :: Num a => Stencil DIM2 a
sobelEdgeX5 = [stencil2| 2   1   0   -1  -2
                         3   2   0   -2  -3
                         4   3   0   -3  -4
                         3   2   0   -2  -3
                         2   1   0   -1  -2|]

sobelEdgeY5 = [stencil2| 2  3  4  3  2
                         1  2  3  2  1
                         0  0  0  0  0
                         -1 -2 -3 -2 -1
                         -2 -3 -4 -3 -2|]


gausianStencilX :: Num a => Stencil DIM2 a
gausianStencilY :: Num a => Stencil DIM2 a
gausianStencilX = [stencil2| 1 4 6 4 1|]
gausianStencilY = [stencil2| 1
                             4
                             6
                             4
                             1 |]

sobelEdgeX7 :: Num a => Stencil DIM2 a
sobelEdgeY7 :: Num a => Stencil DIM2 a
sobelEdgeX7 = [stencil2| 3   2   1   0   -1  -2  -3
                         4   3   2   0   -2  -3  -4
                         5   4   3   0   -3  -4  -5
                         6   5   4   0   -4  -5  -6
                         5   4   3   0   -3  -4  -5
                         4   3   2   0   -2  -3  -4
                         3   2   1   0   -1  -2  -3|]

sobelEdgeY7 = [stencil2| 3  4  5  6  5  4  3
                         2  3  4  5  4  3  2
                         1  2  3  4  3  2  1
                         0  0  0  0  0  0  0
                         -1 -2 -3 -4 -3 -2 -1
                         -2 -3 -4 -5 -4 -3 -2
                         -3 -4 -5 -6 -5 -4 -3 |]

blurGausX :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausY :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausX = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilX
blurGausY = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilY

sobelX :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelY :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelX = mapStencil2 BoundClamp sobelEdgeX
sobelY = mapStencil2 BoundClamp sobelEdgeY

sobelX5 :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelY5 :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelX5 = mapStencil2 BoundClamp sobelEdgeX5
sobelY5 = mapStencil2 BoundClamp sobelEdgeY5

sobelX7 :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelY7 :: (Source r b, Num b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelX7 = mapStencil2 BoundClamp sobelEdgeX7
sobelY7 = mapStencil2 BoundClamp sobelEdgeY7

blur :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blur = blurGausX . blurGausY