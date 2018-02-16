module RepaImage (
  imageToGreyRepa,
  repaToGreyImage,
  repaToRGBImage,
  readIntoRepa
  ) where

import Data.Array.Repa    as R
import Codec.Picture.Repa as C
import Codec.Picture.Types
import Data.Word
import Data.Monoid

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
        create i j           = round (xs ! (Z :. i :. j)) :: Word8


repaToRGBImage :: (RealFrac a, Source r a) => Array r DIM3 a -> Image PixelRGB8
repaToRGBImage arr = generateImage create height width -- may have mixed up the width and height at some point
  where
    Z :. width :. height :. _ = R.extent arr
    create i j                = PixelRGB8 (grab 0) (grab 1) (grab 2)
      where grab k = round $ arr ! (Z :. j :. i :. k) :: Word8