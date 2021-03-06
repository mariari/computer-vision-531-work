module ImageHelper (
  testImage,
  loadRGB,
  loadRGB16,
  loadRGBA16,
  loadRGBA,
  loadRGBJPG,
  imageToGreyMatrix,
  imageToGreyMatrix',
  blurSepX,
  blurSepY,
  blur,
  load,
  matrixToGreyImg
  ) where

import           MatrixHelper
import           Data.Matrix  as M
import           Data.Monoid
import           Codec.Picture
import           Codec.Picture.Types
import qualified Data.Vector.Storable as VS
import           Data.Word


load path = do
  img <- readImage path
  case img of
    Right a -> return a
    Left err -> error ("can't load image: " <> err)


loadRGB :: FilePath -> IO (Image PixelRGB8)
loadRGB = fmap f . load
  where f (ImageRGB8 img) = img
        f _               = error "unsupported format"

loadRGBA :: FilePath -> IO (Image PixelRGBA8)
loadRGBA = fmap f . load
  where f (ImageRGBA8 img) = img
        f _                = error "unsupported format"

loadRGB16 :: FilePath -> IO (Image PixelRGB16)
loadRGB16 = fmap f . load
  where f (ImageRGB16 img) = img
        f _                 = error "unsupported format"

loadRGBA16 :: FilePath -> IO (Image PixelRGBA16)
loadRGBA16 = fmap f . load
  where f (ImageRGBA16 img) = img
        f _                 = error "unsupported format"

loadRGBJPG = fmap f . load
  where f (ImageYCbCr8 img) = img
        f _                 = error "unsupported format"



testImage = loadRGB "../data/test-old.png"

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