module Main where

import MatrixHelper
import ImageHelper
import Codec.Picture.Types
import Codec.Picture
import           Data.Matrix  as M

test = (⊕)

main :: IO ()
main = do
  x <- testImage
  let new  = blur $ fmap fromIntegral (imageToGreyMatrix x)
  print (new ! (1,1))
  print (new ! (1,3))
  print new
--  print (submatrix 1 3 1 3 new)
  let new' = matrixToGreyImg new
  savePngImage "./test-2.png" (ImageY8 (matrixToGreyImg (imageToGreyMatrix x)))
  savePngImage "./test.png" (ImageY8 new')
  print (pixelAt new' 1 1)
