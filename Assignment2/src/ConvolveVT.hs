{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module ConvolveVt
  (
  ) where

import Data.Monoid
import HighLow
import RepaImage as RI
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.Algorithms.Matrix as RM
import Data.Array.Repa.Algorithms.Convolve -- for a stencil this size I'm using the slower version


-- the one given in the python code is wrong, as it uses [1,2,6,2,1]
-- it seems even the lecture is wrong, because if yo add them all up, they
-- don't add up to 256.. can check by summing over my gausian and seeing its 256
gausian :: Array U DIM2 Double
gausian = fromListUnboxed (ix2 5 5) $ (*) . (/ 256)  <$> [1,4,6,4,1] <*> [1,4,6,4,1]


pad :: (Source r e) => e -> DIM2 -> Array r DIM2 e -> Array D DIM2 e
pad val sh vec = fromFunction sh makePad
  where
    Z :. i :. j = R.extent vec
    makePad sh@(Z :. x :. y)
      | x >= i || y >= j = val
      | otherwise      = vec ! sh

testDiff path = do
  img       <- readIntoRepa path
  origV     <- computeVectorP $ R.map fromIntegral (repaRGBToGrey img) :: IO (Array V DIM2 Double)
  origU     <- computeUnboxedP (delay origV)
  convolved <- convolveOutP outClamp gausian origU
  let convolvedC = repaDct (computeVectorS (delay convolved))

  let cosOrigV = repaDct origV
  let cosOrigU = computeUnboxedS (delay cosOrigV)

  paddedGausV <- computeVectorP $ pad 0 (R.extent cosOrigU) gausian
  paddedGausU <- computeUnboxedP (delay $ repaDct paddedGausV)

  matrixMultC <- computeUnboxedP (cosOrigV *^ paddedGausU)
  -- convert it back with idct
  let matrixMult = repaIDct (computeVectorS (delay matrixMultC))

  print ("difference in the DCT "         <> (show (meanDiff matrixMultC convolvedC)))
  print ("difference in the NormalPlane " <> (show (meanDiff matrixMult convolved)))
  return convolved

meanDiff :: (Source r c, Fractional c, Source r2 c) => Array r DIM2 c -> Array r2 DIM2 c -> c
meanDiff arr1 = (/ fromIntegral (i * j)) . sumAllS . R.zipWith (\x y -> abs (x - y)) arr1
  where (Z :. i :. j) = R.extent arr1