{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module ImageCorrelation (
  convolve,
  toGreyP,
  imageCorrelation
  ) where

import RepaImage
import Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Convolve as C
import Data.Array.Repa.Algorithms.Matrix
import qualified Data.Vector.Unboxed as V

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

-- Okay so, Ι was kinda right, Ι just need to figure out how to normalize it
padArray :: Source r e => Int -> Int -> Array r DIM2 e -> Array D DIM2 e
padArray row col arr = R.backpermute (Z :. i + 2 * row  :. j + 2 * col) getClosest arr
  where
    Z :. i :. j = R.extent arr
    getClosest (Z :. row' :. col') = ix2 roff coff
     where coff = min (max (col' - col) 0) (i - 1) -- col' - col is for getting the pos in the old array
           roff = min (max (row' - row) 0) (j - 1) -- from the new coordinates

repaExtractWindows2D :: (Source r a) => Int -> Int -> Array r DIM2 a -> Array D DIM2 (Array D DIM2 a)
repaExtractWindows2D row col arr = R.fromFunction (Z :. i - row :. j - col) grabsubs
  where Z :. i :. j = R.extent arr
        grabsubs sh = R.extract sh (Z :. row :. col) arr

normalizedConv :: (Monad m, Source r Double) => Array r DIM2 Double -> Array U DIM2 Double -> m (Array U DIM2 Double)
normalizedConv arr ker = do
  let Z :. ik :. jk = R.extent ker
  arrExtended      <- R.computeUnboxedP $ padArray (ik `div` 2) (jk `div` 2) arr
  normKern         <- mmultP ker ker
  let normKernSum   = normKern `deepSeqArray` sumAllS normKern
  let extracted     = repaExtractWindows2D ik jk arrExtended -- this should be the same size as arr
  let fn subarr     = sumAllS (subComp *^ ker) / sqrt (normKernSum * sumAllS (mmultS subComp subComp))
        where subComp = computeUnboxedS subarr
  R.computeUnboxedP $ R.map fn extracted


imageCorrelation ::  Double -> FilePath -> FilePath -> IO (Array U DIM2 Double)
imageCorrelation min path1 path2 = do
  x     <- readIntoRepa path1 >>= toGreyP
  y     <- readIntoRepa path2 >>= toGreyP
  let x' = R.map fromIntegral x
  y'    <- R.computeUnboxedP $ R.map fromIntegral y
  convd <- normalizedConv x' y'
  computeUnboxedP $ transpose $ filterMin convd min


filterMin arr min = R.map f arr
  where f x | x >= min  = 255
            | otherwise = 0
{-# INLINE filterMin #-}