{-# LANGUAGE FlexibleContexts #-}
module EdgeDetect
  (
    cannyEdge,
    toJuicyGrey
  ) where

import           Vision.Detector.Edge(canny)
import qualified Vision.Image as I
import           Vision.Image.Conversion
import           Vision.Image.JuicyPixels
import           Codec.Picture.Types
import           GHC.Int

toGrey :: I.RGB -> I.Grey
toGrey = convert

cannyEdge :: Int -> Int32 -> Int32 -> Image PixelRGB8 -> I.Grey
cannyEdge radS min up = canny radS min up . toGrey . toFridayRGB

