{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module RepaImage (
  imageToGreyRepa,
  repaToGreyImage,
  repaToRGBImage,
  readIntoRepa,
  repaRGBToGrey,
  saveRepaGrey
  ) where

import Data.Array.Repa    as R
import Codec.Picture.Repa as C
import Codec.Picture.Types
import Data.Word
import Data.Monoid
import Codec.Picture.Types
import Codec.Picture

imageToGreyRepa :: LumaPlaneExtractable a => Image a -> Array D DIM2 (PixelBaseComponent a)
imageToGreyRepa img@(Image w h _) = R.fromFunction (Z :. w :. h) f
  where f (Z :. i :. j) = pixelAt newImg i j
        newImg          = extractLumaPlane img

readIntoRepa :: FilePath -> IO (Array D DIM3 Word8)
readIntoRepa = fmap f . C.readImageRGB
  where f (Left err)        = error ("Image can't be read: " <> err)
        f (Right (Img dat)) = dat

repaToGreyImage :: (RealFrac a, Source r a) => Array r DIM2 a -> Image Word8
repaToGreyImage xs = generateImage create width height
  where Z :. width :. height = R.extent xs
        create i j           = round (xs ! (Z :. j :. i)) :: Word8


repaToRGBImage :: (RealFrac a, Source r a) => Array r DIM3 a -> Image PixelRGB8
repaToRGBImage arr = generateImage create height width -- may have mixed up the width and height at some point
  where
    Z :. width :. height :. _ = R.extent arr
    create i j                = PixelRGB8 (grab 0) (grab 1) (grab 2)
      where grab k = round $ arr ! (Z :. j :. i :. k) :: Word8

repaRGBToGrey :: (Integral b, Source r b) => Array r DIM3 b -> Array D DIM2 b
repaRGBToGrey arr = R.traverse arr (\_ -> ix2 i j) average
  where (Z :. i :. j :. _)    = R.extent arr
        getAll3 (Z :. i :. j) = (\k -> Z :. i :. j :. k) <$> [0..2]
        average f sh          = round $ sum (fromIntegral . f <$> getAll3 sh) / 3

saveRepaGrey :: (RealFrac a, Source r a) => FilePath -> Array r DIM2 a -> IO ()
saveRepaGrey loc = savePngImage loc . ImageY8 . repaToGreyImage