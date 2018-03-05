module Cosine
    (dcBasis
    , nbyN
    , eightByEight
    ) where

import Data.List.Split
import Normalize
import Control.Applicative
import Data.Matrix

dcBasis :: (Floating p1, Eq p1) => p1 -> p1 -> Int -> Int -> [[p1]]
dcBasis p q m n = chunksOf n $ (*) . (αpq *) <$> ti <*> tj
  where compAlpha x bound
          | x == 0    = 1 / sqrt bound
          | otherwise = sqrt (2 / bound)
        mfloat           = fromIntegral m
        nfloat           = fromIntegral n
        αpq              = compAlpha p mfloat * compAlpha q nfloat
        f offset bound x = cos $ (pi * offset * (2 * x + 1)) / (2 * bound) -- x is an element of a vector
        ti               = f p mfloat . fromIntegral <$> [0..(n-1)]
        tj               = f q nfloat . fromIntegral <$> [0..(m-1)]

nbyN :: (Floating a, Eq a, Enum a) => Int -> [[[[a]]]]
nbyN n = chunksOf n $ (\x y -> dcBasis x y n n) <$> [0..(fromIntegral n-1)] <*> [0..(fromIntegral n-1)]

eightByEight :: [[[[Double]]]]
eightByEight = nbyN 8
