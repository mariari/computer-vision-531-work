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
  img <- readImage "../data/test-image.png"
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

gausianConst :: Num a => [a]
gausianConst = [1,4,6,4,1]

-- this would be easier to work with if Ι just used REPA, they have boundClamp defined for you
-- this could be made a lot faster

blurSepX :: (Num a1, RealFrac a2) => Matrix a2 -> Matrix a1
blurSepX mat = matrix row col f
  where
    gausblur  = fromList 5 1 gausianConst
    clampU    = colVector $ getCol 1   mat -- this gives us the clamp border effect
    clampD    = colVector $ getCol col mat
    buffered  = (clampU <|> clampU) <|> mat <|> (clampD <|> clampD)
    extracted = extractWindows 1 5 buffered
    row       = nrows mat
    col       = ncols mat
    blur p    = sum $ (extracted ! p) * gausblur
    f p       = fromIntegral . round $ (blur p / 16)

blurSepY :: (Num a1, RealFrac a2) => Matrix a2 -> Matrix a1
blurSepY mat = matrix row col f
  where
    gausblur  = fromList 1 5 gausianConst
    zeros     = fromList 2 col (repeat 0)
    clampL    = rowVector $ getRow 1   mat
    clampR    = rowVector $ getRow row mat
    buffered  = (clampL <-> clampL) <-> mat <-> (clampR <-> clampR)
    extracted = extractWindows 5 1 buffered
    row       = nrows mat
    col       = ncols mat
    blur p    = sum $ gausblur * (extracted ! p)
    f p       = fromIntegral . round $ (blur p / 16)


blur :: (Num a1, RealFrac a2) => Matrix a2 -> Matrix a1
blur = blurSepY . blurSepX