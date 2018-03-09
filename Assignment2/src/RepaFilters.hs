{-# LANGUAGE FlexibleContexts #-}
module RepaFilters
  (repaVecComp,
   repaDctImage,
   repaIDctImage,
   repaDctImageP,
   repaIDctImageP,
   repaDct,
   repaIDct,
   repaFft,
   repaIFft,
   repaFftP,
   repaIFftP,
   offsetFft
  ) where

import qualified Data.Vector as V
import           Data.Array.Repa as R
import           Data.Array.Repa.Eval
import           Data.Array.Repa.Repr.Vector
import           Data.Array.Repa.Specialised.Dim2
import           Statistics.Transform
import           Data.Complex

-- DCT ============================================================================================
repaVecComp :: Shape sh => (V.Vector e1 -> V.Vector e2) -> Array V sh e1 -> Array V sh e2
repaVecComp f arr = fromVector (extent arr) (f (toVector arr))

repaDctImage :: Shape sh => Array V sh Double -> Array V sh Double
repaDctImage = repaVecComp (dct . fmap (+ (-128)))

-- For some reason repaDct . repaIDct doesn't give me the identity
-- Ι have to divide it by twice the length
repaIDctImage :: Shape sh => Array V sh Double -> Array V sh Double
repaIDctImage = repaVecComp (normalize . idct)
  where normalize v = (+ 128) . (/ (fromIntegral (length v) * 2)) <$> v

repaDctImageP :: (Monad m, Load r sh Double) => Array r sh Double -> m (Array V sh Double)
repaDctImageP = fmap repaDctImage . computeVectorP

repaIDctImageP :: (Monad m, Load r sh Double) => Array r sh Double -> m (Array V sh Double)
repaIDctImageP = fmap repaIDctImage . computeVectorP

-- These versions don't offset ==================================================

repaDct :: Shape sh => Array V sh Double -> Array V sh Double
repaDct = repaVecComp dct

-- For some reason repaDct . repaIDct doesn't give me the identity
-- Ι have to divide it by twice the length
repaIDct :: Shape sh => Array V sh Double -> Array V sh Double
repaIDct = repaVecComp ((\v -> fmap (/ (fromIntegral (length v) * 2)) v) . idct)

-- Test for DCT ==================================================================================

testsame :: Bool
testsame = (round <$> toList (id vec)) == [1,2,3,4]
  where id  = repaIDctImage . repaDctImage
        vec = fromVector (ix2 2 2) (V.fromList [1,2,3,4])


-- Fft ============================================================================================
repaFft :: Shape sh => Array V sh Double -> Array V sh (Complex Double)
repaFft = repaVecComp (fft . fmap (:+ 0))

repaIFft :: Shape sh => Array V sh (Complex Double) -> Array V sh Double
repaIFft = repaVecComp (fmap magnitude . ifft)


repaFftP :: (Monad m, Load r sh Double) => Array r sh Double -> m (Array V sh (Complex Double))
repaFftP = fmap repaFft . computeVectorP

repaIFftP :: (Monad m, Load r sh (Complex Double)) => Array r sh (Complex Double) -> m (Array V sh Double)
repaIFftP = fmap repaIFft . computeVectorP

offsetFft :: Source r b => Array r DIM2 b -> Array D DIM2 b
offsetFft arr = R.traverse arr id f
  where sh@(Z :. i :. j) = extent arr
        f index (Z :. x :. y)
          | isInside2 sh newShape   = index newShape
          | x + 2 >= i && y + 2 >= j = index (ix2 (x + 2 - i) (y + 2 - j))
          | x + 2 >= i              = index (ix2 (x + 2 - i) (y + 2))
          | otherwise               = index (ix2 (x + 2)     (y + 2 - j))
         where newShape = ix2 (x + 2) (y + 2)