module ImageCorrelationCv (
  correlate,
--  testIO
  ) where

import OpenCV.ImgProc.ObjectDetection
import OpenCV.Juicy
import ImageHelper
import Codec.Picture.Types
import Control.Monad.Except
import qualified OpenCV as CV

cvMatrix8 = fmap (fromImage . extractLumaPlane) . loadRGBJPG

cvMatrix16 = fmap (fromImage . extractLumaPlane) . loadRGB16


correlate arr ker = do
  arr' <- cvMatrix8 arr
  ker' <- cvMatrix8 ker
  let correlation = runExcept $ matchTemplate arr' ker' MatchTemplateCCoeff MatchTemplateNormed
  case correlation of
    Left _ -> error "erorr in transformation"
    Right m -> CV.withWindow "test" $ \win -> CV.imshow win arr'