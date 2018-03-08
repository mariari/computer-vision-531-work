{-# LANGUAGE FlexibleContexts #-}
module HighLow
  (repaIDct,
   repaDct,
   highPass,
   lowPass,
   computeAbsDiff
  ) where
import           Data.Monoid
import           Data.Array.Repa.Repr.Vector
import           Data.Array.Repa as R
import           Statistics.Transform -- This failed me
import qualified Data.Vector as V
import           Foreign.Storable
import qualified Data.Array.CArray as C
import qualified Data.Array.IArray as Arr
import           RepaImage as RI
import           RepaFilters

testsame :: Bool
testsame = (round <$> toList (id vec)) == [1,2,3,4]
  where id  = repaIDctImage . repaDctImage
        vec = fromVector (ix2 2 2) (V.fromList [1,2,3,4])


genPass :: (Source r b, Num b) => (Int -> Int -> Bool) -> Int -> Array r DIM2 b -> Array D DIM2 b
genPass (<>) n arr = R.traverse arr id shrink
  where shrink f sh@(Z :. i :. j)
          | i <> n && j <> n = f sh
          | otherwise       = 0

lowPass :: (Source r b, Num b) => Int -> Array r DIM2 b -> Array D DIM2 b
lowPass = genPass (<=)

highPass :: (Source r b, Num b) => Int -> Array r DIM2 b -> Array D DIM2 b
highPass = genPass (>=)

computeAbsDiff file passedName filter num = do
  x <- readIntoRepa file
  y <- computeVectorP $ R.map fromIntegral (repaRGBToGrey x) :: IO (Array V DIM2 Double)

  let cosY = repaDctImage y
  let fileName = passedName <> "-" <> show num <> ".png"

  passThrough <- repaIDctImageP (filter num cosY) >>= computeUnboxedP . delay
  difference  <- R.computeUnboxedP $ R.map abs (y -^ passThrough)

  saveRepaGrey fileName passThrough
  saveRepaGrey ("abs-diff-" <> fileName) difference


-- Deprecated attempt
repaToCArray :: (Source r e, Storable e) => Array r DIM2 e -> C.CArray (Int, Int) e
repaToCArray arr = Arr.listArray ((0,0), (i - 1 ,j - 1)) (toList arr)
  where (Z :. i :. j) = R.extent arr