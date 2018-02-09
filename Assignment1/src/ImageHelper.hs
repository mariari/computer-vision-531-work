module ImageHelper (
  testImage,
  imageToGreyMatrix,
  imageToGreyMatrix'
  ) where

import           MatrixHelper
import           Data.Matrix  as M
import           Data.Monoid
import qualified Data.Vector as V
import           Codec.Picture
import qualified Data.Vector.Storable as VS
import           Codec.Picture.Types

testImage :: IO (Image PixelRGB8)
testImage = do
  img <- readImage "../data/testImage-image.png"
  case img of
    Right (ImageRGB8 img) -> return img
    Left err -> error ("can't load image: " <> err)
    Right _ -> error "unsupported format"


imageToGreyMatrix :: LumaPlaneExtractable a => Image a -> Matrix (PixelBaseComponent a)
imageToGreyMatrix img = matrix (imageWidth img) (imageHeight img) f
  where
    newImg  = extractLumaPlane img           -- turns the image into greyscale
    f (x,y) = pixelAt newImg (x - 1) (y - 1) -- matrix is 1 indexed not 0

-- fusion does not happen, so this is slower than the non ' version
imageToGreyMatrix' img = fromList (imageWidth img) (imageHeight img) newVec
  where
    newVec  = VS.toList . imageData . extractLumaPlane $ img

blur = undefined