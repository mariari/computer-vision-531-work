{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module RepaHelper (
  blurGausX,
  blurGausY,
  blur,
  repaExtractWindows,
  blurCol,
  blurZ,
  edgeCol,
  sobel,
  edgeColMinP,
  edgeColMinS,
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Array.Repa                   as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Codec.Picture
import RepaImage
-- DATA TYPES==============================================================================
-- just a quick way to distingush between images with 1,3, or 4 words
data MyImage a = RGB a a a | RGBA a a a a | Grey a

fromList :: [a] -> MyImage a
fromList [a,b,c]   = RGB a b c
fromList [a,b,c,d] = RGBA a b c d
fromList [a]       = Grey a
fromList _         = error "not a valid image"

sobelEdgeX :: Num a => Stencil DIM2 a
sobelEdgeY :: Num a => Stencil DIM2 a
sobelEdgeX = [stencil2| -1 0 1
                        -2 0 2
                        -1 0 1|]

sobelEdgeY = [stencil2| -1 -2 -1
                         0  0  0
                         1  2  1|]

gausianStencilX :: Num a => Stencil DIM2 a
gausianStencilY :: Num a => Stencil DIM2 a
gausianStencilX = [stencil2| 1 4 6 4 1|]
gausianStencilY = [stencil2| 1
                             4
                             6
                             4
                             1 |]
-- Working on Grey Images=================================================================
blurGausX :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausY :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausX = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilX
blurGausY = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilY

sobelX :: (Source r b, Num b) => Array r DIM2 b -> Array D DIM2 b
sobelY :: (Source r b, Num b) => Array r DIM2 b -> Array D DIM2 b
sobelX = delay . mapStencil2 BoundClamp sobelEdgeX
sobelY = delay . mapStencil2 BoundClamp sobelEdgeY

blur :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blur = blurGausX . blurGausY

blurZ :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurZ = blur

sobel :: (Source r b, Num b) => Array r DIM2 b -> Array D DIM2 b
sobel = sobelX . sobelY

-- Working on Colored Image and Grey Images===============================================
repaExtractWindows :: (Source r a) => Int -> Int -> Array r DIM3 a -> Array D DIM3 (Array D DIM3 a)
repaExtractWindows row col arr = R.fromFunction (Z :. i - row :. j - col :. k) grabsubs
  where Z :. i :. j :. k = R.extent arr
        grabsubs sh      = R.extract sh (Z :. row :. col :. 1) arr

with2d f = flip reshape . g . fromList . fmap f . slices <*> R.extent
  where g (RGBA a b c d) = interleave4 a b c d
        g (RGB a b c)    = interleave3 a b c
        g (Grey a)       = a

blurCol :: (Fractional e, Source r e) => Array r DIM3 e -> Array D DIM3 e
blurCol = with2d blur

edgeCol :: (Fractional e, Source r e) => Array r DIM3 e -> Array D DIM3 e
edgeCol = with2d sobel

slices :: Source r e => Array r DIM3 e -> [Array D DIM2 e]
slices arr = f <$> [0..(k-1)]
  where
    Z :. _ :. _ :. k = R.extent arr
    f a              = slice arr (Z :. All :. All :. (a :: Int))

-- Filter a pixel of an array if it is below a certain amount
-- here we are going to make a new array via making a new one and traversing

-- Sadly can't make a generic one due to how the type signatures turned out... so that's a shame
filterPixelsS :: (Source r b, Fractional b, Ord b, V.Unbox b) => Array r DIM3 b -> b -> Array D DIM3 b
filterPixelsS arr min = R.traverse arr id (passThresh min averaged)
  where
    (Z :. _ :. _ :. k) = R.extent arr
    averaged           = R.sumS $ R.map (/ fromIntegral k) arr

filterPixelsP :: (Monad m, Source r b, Fractional b, Ord b, V.Unbox b) => Array r DIM3 b -> b -> m (Array D DIM3 b)
filterPixelsP arr min = R.traverse arr id . passThresh min <$> averaged
  where
    (Z :. _ :. _ :. k) = R.extent arr
    averaged           = R.sumP $ R.map (/ fromIntegral k) arr

-- helper for filterPixelP and S
passThresh min avg f sh@(Z :. i :. j :. _) | avg ! ix2 i j >= min = f sh
                                           | otherwise            = 0

edgeColMinS :: (Source r b, V.Unbox b, Ord b, Fractional b) => Array r DIM3 b -> b -> Array D DIM3 b
edgeColMinS = filterPixelsS . edgeCol

edgeColMinP :: (Monad m, Source r b, V.Unbox b, Ord b, Fractional b) => Array r DIM3 b -> b -> m(Array D DIM3 b)
edgeColMinP = filterPixelsP . edgeCol

-- Testing functions=====================================================================

testIO = do
  z <- readIntoRepa "../Assignment1/data/test-image.png"
  let z' = blurCol (R.map fromIntegral z) :: Array D DIM3 Double
  print (z' ! (Z :. 1 :. 1 :. 0))
  print (z' ! (Z :. 1 :. 1 :. 1))
  print (z' ! (Z :. 1 :. 1 :. 2))
