{-# LANGUAGE BangPatterns, QuasiQuotes #-}
module RepaHelper (
  imageToGreyRepa,
  blurGausX,
  blurGausY,
  blur,
  repaToGreyImage
  ) where
import Data.Array.Repa                   as R
import Data.Array.Repa.Stencil           as R
import Data.Array.Repa.Stencil.Dim2      as RD
import ImageHelper(testImage)
import Codec.Picture
import Codec.Picture.Types
import Data.Word

-- only going to be working on 2D images for now, trying to figure out slices is too much
imageToGreyRepa :: LumaPlaneExtractable a => Image a -> Array D DIM2 (PixelBaseComponent a)
imageToGreyRepa img@(Image w h d) = R.fromFunction (Z :. w :. h) f
  where f (Z :. i :. j) = pixelAt newImg i j
        newImg          = extractLumaPlane img


gausianStencilX :: Num a => Stencil DIM2 a
gausianStencilX = [stencil2| 1 4 6 4 1 |]

gausianStencilY :: Num a => Stencil DIM2 a
gausianStencilY = [stencil2| 1
                             4
                             6
                             4
                             1 |]
blurGausX :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausX = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilX


blurGausY :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausY = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilY

blur :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blur = blurGausX . blurGausY

repaToGreyImage :: (RealFrac a, Source r a) => Array r DIM2 a -> Image Word8
repaToGreyImage xs = generateImage create width height
  where Z :. width :. height = R.extent xs
        create i j           = fromIntegral (round (xs ! (Z :. i :. j))) :: Word8