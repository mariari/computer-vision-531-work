{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module HighLow
  (repaIDct,
   repaDct
  ) where

import            Data.Array.Repa.Repr.Vector
import            Data.Array.Repa as R
import            Statistics.Transform -- This failed me
import qualified  Data.Vector as V
import            RepaImage
import            Foreign.Storable
import qualified Data.Array.CArray as C
import qualified Data.Array.IArray as Arr


-- x <- readIntoRepa "data/Boats.png"
-- y <- computeVectorP $ R.map (fromIntegral) (repaRGBToGrey x) :: IO (Array V DIM2 Double)
-- cosY = repaDct y


-- For some reason repaDct . repaIDct doesn't give me the identity
repaVecComp :: Shape sh => (V.Vector e1 -> V.Vector e2) -> Array V sh e1 -> Array V sh e2
repaVecComp f arr = fromVector (R.extent arr) (f (toVector arr))

repaIDct :: Shape sh => Array V sh Double -> Array V sh Double
repaIDct = repaVecComp idct

repaDct :: Shape sh => Array V sh Double -> Array V sh Double
repaDct = repaVecComp dct


-- let y = computeS $ R.map (fromIntegral) (repaRGBToGrey x)
-- let z = repaToCArray (repaRGBToGrey x)
-- Odd this does not work
-- z' = dct3 $ dct2 ( z)
repaToCArray :: (Source r e, Storable e) => Array r DIM2 e -> C.CArray (Int, Int) e
repaToCArray arr = Arr.listArray ((0,0), (i - 1 ,j - 1)) (toList arr)
  where (Z :. i :. j) = R.extent arr