module Cosine
    (dcBasis
    ) where
import Data.List.Split

dcBasis :: (Floating p1, Eq p1) => p1 -> p1 -> Int -> Int -> [[p1]]
dcBasis p q m n = chunksOf n $ (*) . (αp * αq *) <$> ti <*> tj
  where compAlpha x bound
          | x == 0    = 1 / sqrt bound
          | otherwise = sqrt (2 / bound)
        mfloat           = fromIntegral m
        nfloat           = fromIntegral n
        αp               = compAlpha p mfloat
        αq               = compAlpha q nfloat
        f offset bound x = cos $ (pi * offset * (2 * x + 1)) / 2 / bound -- x is an element of a vector
        ti               = f p mfloat . fromIntegral <$> [0..(n-1)]
        tj               = f q nfloat . fromIntegral <$> [0..(m-1)]

infixl 3 <$$>
(<$$>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap