module Cosine
    (discreteBase,
     meshgrid
    ) where

discreteBase :: (Integral p, Floating p1, Eq p1) => p1 -> p1 -> p -> p -> [[p1]]
discreteBase p q m n = (αp * αq *) <$$> zipWith (zipWith (*)) ti tj
  where compAlpha x bound
          | x == 0    = 1 / sqrt bound
          | otherwise = sqrt (2 / bound)
        (mfloat,nfloat) = (fromIntegral m, fromIntegral n)
        αp              = compAlpha p mfloat
        αq              = compAlpha q nfloat
        (ibasis,jbasis) = meshgrid [0..(n-1)] [0..(m-1)]
        f offset bound x = cos $ (pi * offset * (2 * x + 1)) / 2 / bound  -- x is an element of a vector
        ti               = f p mfloat . fromIntegral <$$> ibasis
        tj               = f q nfloat . fromIntegral <$$> jbasis

meshgrid :: (Functor f, Foldable f, Foldable t) => t a1 -> f a2 -> ([t a1], f [a2])
meshgrid xs ys = (replicate (length ys) xs,replicate (length xs) <$> ys)

infixl 3 <$$>
(<$$>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap