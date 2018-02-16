module Main where

import RepaImage as R
import ImageHelper
import ImageCorrelation
import qualified RepaHelper as R
import qualified Data.Array.Repa as R
import Codec.Picture.Types
import Codec.Picture
import Codec.Picture.Repa               as C
import EdgeDetect
import qualified Vision.Image as I
--main :: IO ()

main = do
  mainCorr

mainCorr = do
  x <- imageCorrelation 254 "./data/object/soccer-kernel.jpg" "./data/object/soccer_balls.jpg"
  x' <- R.computeUnboxedP $ R.map fromIntegral x :: IO (R.Array R.U R.DIM2 Double)
  let x'' = R.repaToGreyImage x'
  savePngImage "ImageCorrTest.png" (ImageY8 x'')

--runs in about 20 seconds
mainCanny = do
  x <- C.readImageRGB "./data/Trees.jpg"
  let y = case x of Left _ -> error "image not found"; Right z -> z
  -- blurring
  let z = R.blurCol (R.map fromIntegral (imgData y))
  parallelB <- R.computeUnboxedP z :: IO(R.Array R.U R.DIM3 Double)
  let z' = R.repaToRGBImage parallelB
  -- Canny Algo
  parallelC <- I.computeP (cannyEdge 5 220 300 z')
  let z''' = toJuicyGrey parallelC
  -- Save
  savePngImage "./Color-save.png" (ImageY8 z''')


-- Runs in about 20 seconds
mainRepa = do
  x <- C.readImageRGB "./data/Color-test.png"
  let y = case x of Left _ -> error "image not found"; Right z -> z
  -- Blurring
  let z = R.blurCol (R.map fromIntegral (imgData y))
  z'  <- R.computeUnboxedP z :: IO(R.Array R.U R.DIM3 Double)
  -- Sobel Edge Detection
  z'' <- R.edgeColMinP z' 100
  computed <- R.computeUnboxedP z'' :: IO(R.Array R.U R.DIM3 Double)
  let z''' = R.repaToRGBImage computed
  -- Save
  savePngImage "./Color-save.png" (ImageRGB8 z''')

mainRepaGrey = do
  x <- testImage
  let y = R.imageToGreyRepa x
  let z = R.blur $ R.map fromIntegral y
  savePngImage "./repa-test-real.png" (ImageY8 (R.repaToGreyImage z))

mainMatrix :: IO ()
mainMatrix = do
  x <- testImage
  let new  = blur $ fmap fromIntegral (imageToGreyMatrix x)
  let new' = matrixToGreyImg new
  savePngImage "./test-2.png" (ImageY8 (matrixToGreyImg (imageToGreyMatrix x)))
  savePngImage "./test.png" (ImageY8 new')
  print (pixelAt new' 1 1)
