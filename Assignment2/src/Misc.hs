module Misc
  ((<$$>)
  ) where

infixl 3 <$$>
(<$$>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap