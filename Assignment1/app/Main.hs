module Main where

import ImageHelper
import qualified RepaHelper as R
import qualified Data.Array.Repa as R
import Codec.Picture.Types
import Codec.Picture
import Codec.Picture.Repa               as C

--main :: IO ()
main = do
  x <- C.readImageRGB "./data/Color-test.png"
  let y = case x of Left _ -> error "image not found"; Right z -> z
  let z = R.blurCol (R.map fromIntegral (imgData y))
  z' <- R.computeUnboxedP z :: IO(R.Array R.U R.DIM3 Double)
  z'' <- R.edgeColMinP z' 210
  computed <- R.computeUnboxedP z'' :: IO(R.Array R.U R.DIM3 Double)
  let z''' = R.repaToRGBImage computed
  savePngImage "./Color-save.png" (ImageRGB8 z''')
--  print $ z R.! (R.Z R.:. 1 R.:. 1 R.:. 1)
--  print $ z R.! (R.Z R.:. 1 R.:. 1 R.:. 0)
--  print $ z R.! (R.Z R.:. 1 R.:. 1 R.:. 2)

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
