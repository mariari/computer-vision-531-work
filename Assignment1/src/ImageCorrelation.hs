{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module ImageCorrelation (
  convolve,
  toGreyP,
  imageCorrelation
    ) where

import RepaImage
import Data.Array.Repa as R
import Codec.Picture.Types
import Data.Word
import RepaHelper
import Data.Array.Repa.Algorithms.Convolve as C
import Control.Monad.Identity
import ImageHelper as I
import qualified Data.Vector.Unboxed as V
import Codec.Picture.Repa                as C

-- Yay Ι don't have to figure out FFT's and DFT's just yet!
-- we can do this without it

-- we can just use outClamp to get the closet image

-- so runIdentity basically tells convolveOutP to run the Identity monad instead of forcing me into IO
-- Ι really hope this keeps parallelism
convolve :: (Num a, V.Unbox a, Monad m) => Array U DIM2 a -> Array U DIM2 a -> m (Array U DIM2 a)
convolve kernel = convolveOutP outClamp kernel


-- cna run the identity monad if you want to unbox  this
toGreyP :: (Monad m, V.Unbox a, Integral a, Source r a) => Array r DIM3 a -> m (Array U DIM2 a)
toGreyP arr = R.sumP $ R.map (`div` fromIntegral k) arr
  where (Z :. _ :. _ :. k) = R.extent arr

--filterMin :: (Functor f, Shape sh, Source r p, Ord p, Num p) => f (Array r sh p) -> p -> f (Array D sh p)
filterMin arr min = R.map f arr
  where f x | x >= min  = x
            | otherwise = 0
{-# INLINE filterMin #-}

imageCorrelation ::  Word8 -> FilePath -> FilePath -> IO (Array U DIM2 Word8)
imageCorrelation min path1 path2 = do
  x <- readIntoRepa path1 >>= toGreyP
  y <- readIntoRepa path2 >>= toGreyP
  convd <- convolve x y
  computeUnboxedP $ transpose $ filterMin convd min

-- the code above is all wrong, need to figure out SIFT and affine
