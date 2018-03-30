{-# LANGUAGE BangPatterns
            ,DataKinds
            ,ScopedTypeVariables
            ,TypeOperators
            ,TupleSections
            ,TypeFamilies
            ,FlexibleContexts#-}

module Minst
  ( MNIST,
    mai
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
import           Grenade.Utils.OneHot as OH


type MNIST
  = Network
    '[ Convolution 1 10 5 5 1 1, Pooling 2 2 2 2, Relu
     , Convolution 10 16 5 5 1 1, Pooling 2 2 2 2, Reshape, Relu
     , FullyConnected 256 80, Logit, FullyConnected 80 10, Logit]
    '[ 'D2 28 28,                'D3 24 24 10,     'D3 12 12 10
     , 'D3 12 12 10,             'D3 8 8 16,       'D3 4 4 16 , 'D1 256
     , 'D1 256 , 'D1 80, 'D1 80, 'D1 10, 'D1 10]
  

randomMnist :: MonadRandom m => m MNIST
randomMnist = randomNetwork


convTest :: Int -> FilePath -> FilePath -> LearningParameters -> ExceptT String IO ()
convTest iterations trainFile validateFile rate = do
  net0         <- lift randomMnist
  trainData    <- readMNIST trainFile
  validateData <- readMNIST validateFile
  lift $ foldM_ (runIteration trainData validateData) net0 [1..iterations]

    where
  trainEach rate' !network (i, o) = train rate' network i o

  runIteration trainRows validateRows net i = do
    let trained' = foldl' (trainEach ( rate { learningRate = learningRate rate * 0.9 ^ i} )) net trainRows
    let res      = fmap (\(rowP,rowL) -> (rowL,) $ runNet trained' rowP) validateRows
    let res'     = fmap (\(S1D label, S1D prediction) -> (maxIndex (SA.extract label), maxIndex (SA.extract prediction))) res
    print trained'
    putStrLn $ "Iteration " ++ show i ++ ": " ++ show (length (filter ((==) <$> fst <*> snd) res')) ++ " of " ++ show (length res')
    return trained'

data MnistOpts = MnistOpts FilePath FilePath Int LearningParameters

mnist' :: Parser MnistOpts
mnist' = MnistOpts <$> argument str (metavar "TRAIN")
                   <*> argument str (metavar "VALIDATE")
                   <*> option auto (long "iterations" <> short 'i' <> value 15)
                   <*> (LearningParameters
                       <$> option auto (long "train_rate" <> short 'r' <> value 0.01)
                       <*> option auto (long "momentum" <> value 0.9)
                       <*> option auto (long "l2" <> value 0.0005))

mai :: IO ()
mai = do
    MnistOpts mnist vali iter rate <- execParser (info (mnist' <**> helper) idm)
    putStrLn vali
    putStrLn "Training convolutional neural network..."

    res <- runExceptT $ convTest iter mnist vali rate
    case res of
      Right () -> pure ()
      Left err -> putStrLn err

readMNIST :: FilePath -> ExceptT String IO [(S ('D2 28 28), S ('D1 10))]
readMNIST mnist = ExceptT $ traverse (A.parseOnly parseMNIST) . T.lines <$> T.readFile mnist

parseMNIST :: A.Parser (S ('D2 28 28), S ('D1 10))
parseMNIST = do
  Just lab <- oneHot <$> A.decimal
  pixels   <- many (A.char ',' >> A.double)
  image    <- maybe (fail "Parsed row was of an incorrect size") pure (fromStorable . V.fromList $ pixels)
  return (image, lab)