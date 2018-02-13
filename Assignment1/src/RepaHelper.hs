{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module RepaHelper (
  imageToGreyRepa,
  blurGausX,
  blurGausY,
  blur,
  repaToGreyImage,
  repaExtractWindows,
  blurCol,
  repaToRGBImage,
  edgeCol
  ) where

import Data.Array.Repa                   as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Codec.Picture
import Codec.Picture.Repa                as C
import Codec.Picture.Types
import Data.Word

-- DATA TYPES==============================================================================
-- just a quick way to distingush between images with 1,3, or 4 words
data MyImage a = RGB a a a | RGBA a a a a | Grey a

fromList :: [a] -> MyImage a
fromList [a,b,c]   = RGB a b c
fromList [a,b,c,d] = RGBA a b c d
fromList [a]       = Grey a
fromList _         = error "not a valid image"


-- only going to be working on 2D images for now, trying to figure out slices is too much
imageToGreyRepa :: LumaPlaneExtractable a => Image a -> Array D DIM2 (PixelBaseComponent a)
imageToGreyRepa img@(Image w h _) = R.fromFunction (Z :. w :. h) f
  where f (Z :. i :. j) = pixelAt newImg i j
        newImg          = extractLumaPlane img


sobelEdgeX :: Num a => Stencil DIM2 a
sobelEdgeY :: Num a => Stencil DIM2 a
sobelEdgeX = [stencil2| -1 0 1
                        -2 0 2
                        -1 0 1|]

sobelEdgeY = [stencil2| -1 -2 -1
                         0  0  0
                         1  2  1|]

gausianStencilX :: Num a => Stencil DIM2 a
gausianStencilY :: Num a => Stencil DIM2 a
gausianStencilX = [stencil2| 1 4 6 4 1|]
gausianStencilY = [stencil2| 1
                             4
                             6
                             4
                             1 |]
-- Working on Grey Images=================================================================
blurGausX :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausY :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blurGausX = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilX
blurGausY = R.map (/ 16) . mapStencil2 BoundClamp gausianStencilY

sobelX :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
sobelY :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
sobelX = R.map (/ 1) . mapStencil2 BoundClamp sobelEdgeX
sobelY = R.map (/ 1) . mapStencil2 BoundClamp sobelEdgeY

blur :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
blur = blurGausX . blurGausY

sobel :: (Source r b, Fractional b) => Array r DIM2 b -> Array D DIM2 b
sobel = sobelX . sobelY

repaToGreyImage :: (RealFrac a, Source r a) => Array r DIM2 a -> Image Word8
repaToGreyImage xs = generateImage create width height
  where Z :. width :. height = R.extent xs
        create i j           = round (xs ! (Z :. i :. j)) :: Word8

-- Working on Colored Image and Grey Images===============================================
repaExtractWindows :: (Source r a) => Int -> Int -> Array r DIM3 a -> Array D DIM3 (Array D DIM3 a)
repaExtractWindows row col arr = R.fromFunction (Z :. i - row :. j - col :. k) grabsubs
  where Z :. i :. j :. k = R.extent arr
        grabsubs sh      = R.extract sh (Z :. row :. col :. 1) arr


--with2d :: (Fractional e, Source r e) => Array r DIM3 e -> Array D DIM3 e
with2d f = flip reshape . g . fromList . fmap f . slices <*> R.extent
  where g (RGBA a b c d) = interleave4 a b c d
        g (RGB a b c)    = interleave3 a b c
        g (Grey a)       = a

blurCol :: (Fractional e, Source r e) => Array r DIM3 e -> Array D DIM3 e
blurCol = with2d blur


edgeCol :: (Fractional e, Source r e) => Array r DIM3 e -> Array D DIM3 e
edgeCol = with2d sobel

slices :: Source r e => Array r DIM3 e -> [Array D DIM2 e]
slices arr = f <$> [0..(k-1)]
  where
    (Z :. _ :. _ :. k) = R.extent arr
    f a                = slice arr (Z :. All :. All :. (a :: Int))


repaToRGBImage :: (RealFrac a, Source r a) => Array r DIM3 a -> Image PixelRGB8
repaToRGBImage arr = generateImage create height width -- may have mixed up the width and height at some point
  where
    Z :. width :. height :. _ = R.extent arr
    create i j                = PixelRGB8 (grab 0) (grab 1) (grab 2)
      where grab k = round $ arr ! (Z :. j :. i :. k) :: Word8

-- Testing functions=====================================================================

testIO = do
  x <- C.readImageRGB "../Assignment1/data/test-image.png"
  let y = case x of Left _ -> undefined; Right z -> z
  let z = imgData y
  let z' = blurCol (R.map fromIntegral (imgData y)) :: Array D DIM3 Double
  print (z' ! (Z :. 1 :. 1 :. 0))
  print (z' ! (Z :. 1 :. 1 :. 1))
  print (z' ! (Z :. 1 :. 1 :. 2))
  print (computeUnboxedS (slice z' (Z :. (1 :: Int) :. (1 :: Int) :. All)))
  print (computeUnboxedS $ R.extract (Z :. 1 :. 1 :. 0) (Z :. 5 :. 1 :. 3) z')
  let f x = R.computeUnboxedS (slice (repaExtractWindows 5 1 z') (Z :. (1 :: Int) :. (1 :: Int) :. All) ! (Z :. x))
  print (f 0)
  print (f 1)
  print (f 2)
  let z'' = repaToRGBImage z'
  print $ pixelAt z'' 1 1
