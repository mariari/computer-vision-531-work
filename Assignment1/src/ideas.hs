module Matrix.Extras (
  linearFilter,
    (⊕)
  ) where

import           Data.Matrix  as M
import           Control.Lens as L
import           Data.Monoid
import qualified Data.Vector as V
import           Codec.Picture
import           Data.Foldable as F
import qualified Data.Vector.Storable as VS
import           Codec.Picture.Types
-- DATA ==========================================================================


-- TestImage data =====================================================================
matrix310 :: (Num a, Enum a) => Matrix a
matrix310 = fromLists [[45,60,98,127,132,133,137,133]
                      ,[46,65,98,123,126,128,131,133]
                      ,[47,65,96,115,119,123,135,137]
                      ,[47,63,91,107,113,122,138,134]]

fromBookOffset = fromList 3 3 $ replicate 4 0.1 <> (0.2 : repeat 0.1)

-- Functions ======================================================================
extractWindows :: Int -> Int -> Matrix a -> Matrix (Matrix a)
extractWindows row col mat = matrix (nrows mat - row + 1) (ncols mat - col + 1) f
  where
    f (i,j) = submatrix i (i + row - 1) j (j + col - 1) mat

splitFrmaes = extractWindows

linearFilter :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
linearFilter filt = fmap (round . foldr (+) 0 . elementwise (*) filt) . extractWindows row col
  where row = nrows filt
        col = ncols filt

(⊕) :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
(⊕) = flip linearFilter

-- matrix310 ⊕ fromBookOffset

-- Juicy Pixel testImage

-- this testImage is only defined for pixelRGB8
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

main = do
  x <- testImage
  let new  = imageToGreyMatrix x
  print (new ! (1500,1500))


-- compiling stack ghc -- -O2 -RTS -rtsopts ideas.hs
-- running time ./ideas +RTS -sstderr