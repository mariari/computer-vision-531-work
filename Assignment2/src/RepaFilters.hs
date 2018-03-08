{-# LANGUAGE FlexibleContexts #-}
module RepaFilters
  (repaVecComp,
   repaDctImage,
   repaIDctImage,
   repaDctImageP,
   repaIDctImageP,
   repaDct,
   repaIDct
  ) where
import           Data.Array.Repa.Repr.Vector
import           Data.Array.Repa.Eval
import           Data.Array.Repa as R
import           Statistics.Transform -- This failed me
import qualified Data.Vector as V


repaVecComp :: Shape sh => (V.Vector e1 -> V.Vector e2) -> Array V sh e1 -> Array V sh e2
repaVecComp f arr = fromVector (R.extent arr) (f (toVector arr))

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

-- These versions don't offset the image
repaDct :: Shape sh => Array V sh Double -> Array V sh Double
repaDct = repaVecComp dct

-- For some reason repaDct . repaIDct doesn't give me the identity
-- Ι have to divide it by twice the length
repaIDct :: Shape sh => Array V sh Double -> Array V sh Double
repaIDct = repaVecComp ((\v -> fmap (/ (fromIntegral (length v) * 2)) v) . idct)