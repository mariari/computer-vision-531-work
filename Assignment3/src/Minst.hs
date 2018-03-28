{-# LANGUAGE BangPatterns
            ,DataKinds
            ,ScopedTypeVariables
            ,TypeOperators
            ,TupleSections
            ,TypeFamilies
            ,FlexibleContexts#-}

module Minst
  ( MNIST
  ) where

import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as A
import           Data.List (foldl')
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as V
import           Numeric.LinearAlgebra ( maxIndex )
import qualified Numeric.LinearAlgebra.Static as SA
import           Options.Applicative
import           Grenade
import           Grenade.Utils.OneHot

type MNIST = Network
             '[ Convolution 1 10 5 5 1 1, Pooling 2 2 2 2, Relu
              , Convolution 10 16 5 5 1 1, Pooling 2 2 2 2, Reshape, Relu
              , FullyConnected 256 80, Logit, FullyConnected 80 10, Logit]
             '[ 'D2 28 28, 'D3 24 24 10, 'D3 12 12 10, 'D3 12 12 10
              , 'D3 8 8 16, 'D3 4 4 16, 'D1 256, 'D1 256
              , 'D1 80, 'D1 80, 'D1 10, 'D1 10]


randomMnist :: MonadRandom m => m MNIST
randomMnist = randomNetwork