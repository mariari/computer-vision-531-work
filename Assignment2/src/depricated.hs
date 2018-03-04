meshgrid :: (Functor f, Foldable f, Foldable t) => t a1 -> f a2 -> ([t a1], f [a2])
meshgrid xs ys = (replicate (length ys) xs,replicate (length xs) <$> ys)