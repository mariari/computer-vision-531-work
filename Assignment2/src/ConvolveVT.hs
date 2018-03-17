{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module ConvolveVt
  (
  ) where

import RepaImage
import HighLow
import RepaFilters
import Data.Monoid
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.Algorithms.Matrix as RM
import Data.Array.Repa.Algorithms.Convolve -- for a stencil this size I'm using the slower version


-- the one given in the python code is wrong, as it uses [1,2,6,2,1]
-- it seems even the lecture is wrong, because if yo add them all up, they
-- don't add up to 256.. can check by summing over my gausian and seeing its 256
gausian :: Array U DIM2 Double
gausian = fromListUnboxed (ix2 5 5) $ (*) . (/ 256) <$> [1,4,6,4,1] <*> [1,4,6,4,1]


pad :: (Source r e) => e -> DIM2 -> Array r DIM2 e -> Array D DIM2 e
pad val sh vec = fromFunction sh makePad
  where
    Z :. i :. j = R.extent vec
    makePad sh@(Z :. x :. y)
      | x >= i || y >= j = val
      | otherwise        = vec ! sh

padOff :: (Source r e) => e -> DIM2 -> Array r DIM2 e -> Int -> Int -> Array D DIM2 e
padOff val sh vec offx offy = fromFunction sh makePad
  where
    Z :. i :. j = R.extent vec
    makePad sh@(Z :. x :. y)
      | x - offx >= i || x - offx < 0
      || y - offy >= j || y - offy < 0 = val
      | otherwise                    = vec ! ix2 (x - offx) (y - offy)

meanDiff :: (Source r c, Fractional c, Source r2 c) => Array r DIM2 c -> Array r2 DIM2 c -> c
meanDiff arr1 = (/ fromIntegral (i * j)) . sumAllS . R.zipWith (\x y -> abs (x - y)) arr1
  where (Z :. i :. j) = R.extent arr1


testDiffGen forwardTransformP inverseTransformP forwardTransform paddingP path = do
  img         <- readIntoRepa path
  let origV    = R.map fromIntegral (repaRGBToGrey img)
  origU       <- computeUnboxedP origV
  convolved   <- convolveOutP outClamp gausian origU
  convolvedC  <- forwardTransformP . delay $ convolved

  cosOrigV    <- forwardTransformP origV
  let cosOrigU = computeUnboxedS (delay cosOrigV)
  let ext@(Z :. i :. j)   = R.extent cosOrigU
  let padding | paddingP  = padOff 0 ext gausian (i `div` 2) (j `div` 2)
              | otherwise = pad    0 ext gausian

  paddedGausV <- computeVectorP $ pad 0 (R.extent cosOrigU) gausian
  paddedGausU <- computeUnboxedP (delay $ forwardTransform paddedGausV)

  let matrixMultC = (cosOrigV *^ paddedGausU)
  -- convert it back with idct
  matrixMult <- inverseTransformP matrixMultC
  saveRepaGrey "test.png" matrixMult
  saveRepaGrey "test2.png" convolved
  print ("difference in the DCT "         <> show (meanDiff matrixMultC convolvedC))
  print ("difference in the NormalPlane " <> show (meanDiff matrixMult convolved))
  return (matrixMult, convolved)

-- we give repaDct as this is for the gaussian, which is not an image
--testDiffDct :: FilePath -> IO ()
testDiffDct = testDiffGen repaDctImageP repaIDctImageP repaDct

--testDiffFft :: FilePath -> IO ()
testDiffFft = testDiffGen repaFftP (fmap (computeVectorS . offsetFft) . repaIFftP) repaFft True