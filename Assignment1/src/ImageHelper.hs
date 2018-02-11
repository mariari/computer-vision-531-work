module ImageHelper (
  testImage,
  imageToGreyMatrix,
  imageToGreyMatrix',
  blurSepX,
  blurSepY,
  blur,
  matrixToGreyImg
  ) where

import           MatrixHelper
import           Data.Matrix  as M
import           Data.Monoid
import qualified Data.Vector as V
import           Codec.Picture
import           Codec.Picture.Types
import qualified Data.Vector.Storable as VS
import           Data.Word

testImage :: IO (Image PixelRGB8)
testImage = do
  img <- readImage "../data/test-old.png"
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
imageToGreyMatrix' :: LumaPlaneExtractable a => Image a -> Matrix (PixelBaseComponent (PixelBaseComponent a))
imageToGreyMatrix' img = fromList (imageWidth img) (imageHeight img) newVec
  where
    newVec  = VS.toList . imageData . extractLumaPlane $ img

gausianConst :: Num a => [a]
gausianConst = [1,4,6,4,1]

-- this would be easier to work with if Ι just used REPA, they have boundClamp defined for you, and has stencil hacks
-- this could be made a lot faster

-- I round up to Word16's
-- also note that this algorithm can be made completely general on Integrals and not just Word8's however, I want speed
blurSepX :: Matrix Word16 -> Matrix Word16
blurSepX mat =  withWord16 (* gausblur) <$> extracted
  where
    clampL    = colVector $ getCol 1           mat -- this gives us the clamp border effect
    clampR    = colVector $ getCol (ncols mat) mat
    buffered  = (clampL <|> clampL) <|> mat <|> (clampR <|> clampR)
    extracted = extractWindows 1 5 buffered
    gausblur  = fromList 5 1 gausianConst

blurSepY :: Matrix Word16 -> Matrix Word16
blurSepY mat = withWord16 (gausblur *)  <$> extracted
  where
    clampU    = rowVector $ getRow 1           mat
    clampD    = rowVector $ getRow (nrows mat) mat
    buffered  = (clampU <-> clampU) <-> mat <-> (clampD <-> clampD)
    extracted = extractWindows 5 1 buffered
    gausblur  = fromList 1 5 gausianConst

withWord16 :: (Matrix Word16 -> Matrix Word16) -> Matrix Word16 -> Word16
withWord16 f mat = (`div` 16) . sum $ f mat16
  where
    mat16 = fromIntegral <$> mat :: Matrix Word16

blur :: Matrix Word16 -> Matrix Word8
blur = fmap fromIntegral . blurSepY . blurSepX

matrixToGreyImg :: Pixel a => Matrix a -> Image a
matrixToGreyImg mat = generateImage f (ncols mat) (nrows mat)
  where f i j = mat ! (i + 1, j + 1)