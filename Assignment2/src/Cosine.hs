module Cosine
    (dcBasis
    , eightByEight
    ) where

import Data.List.Split
import Normalize

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

eightByEight :: [[[[Double]]]]
eightByEight = chunksOf 8 $ (\x y -> dcBasis x y 8 8) <$> [0..7] <*> [0..7]