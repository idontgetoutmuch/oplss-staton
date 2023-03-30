{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module MHSingleRandFree (
  justSample,
  sampleViaMh,
  ) where

import Control.Monad.Writer
import Control.Monad.State
import System.Random hiding (random)

import System.Random.Stateful hiding (random)
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Applicative
import Data.Word
import Data.Bits
import Data.Ratio
import Control.Monad.Trans.Free
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

getrandom :: State [Double] Double
getrandom = do
  ~(r:rs) <- get
  put rs
  return r

data MyGen = MyGen

instance StatefulGen MyGen (State [Double])  where
  uniformWord64 _g = do r <- getrandom
                        let m = fromIntegral (maxBound :: Word64) :: Double
                        return $ fromIntegral $ numerator $ toRational (m * r)
  uniformWord8   g = do r <- uniformWord64 g
                        return $ fromIntegral $ rotate r 8
  uniformShortByteString = error "uniformShortByteString"

instance (MonadTrans t, Monad (t (State [Double]))) =>
         StatefulGen MyGen (t (State [Double]))  where
  uniformWord64 g = lift $ uniformWord64 g
  uniformShortByteString = error "uniformShortByteString"

class Monad m => MonadDistribution m where
  random :: m Double

class Monad m => MonadFactor m where
  score' :: Double -> m ()

newtype Sampler g m a = Sampler (ReaderT g m a) deriving (Functor, Applicative, Monad)

instance StatefulGen g m => MonadDistribution (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

sampleWith :: StatefulGen g m => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  random = lift random

newtype Weighted m a = Weighted (StateT Double m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadDistribution)

instance Monad m => MonadFactor (Weighted m) where
  score' w = Weighted (modify (* w))

class (MonadDistribution m, MonadFactor m) => MonadMeasure m

weighted :: Weighted m a -> m (a, Double)
weighted (Weighted m) = runStateT m 1

unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

data Trace a = Trace { variables :: [Double], output :: a, probDensity :: Double }

instance Functor Trace where
  fmap f t = t {output = f (output t)}

instance Applicative Trace where
  pure x = Trace {variables = [], output = x, probDensity = 1}
  tf <*> tx =
    Trace
      { variables = variables tf ++ variables tx,
        output = output tf (output tx),
        probDensity = probDensity tf * probDensity tx
      }

instance Monad Trace where
  t >>= f =
    let t' = f (output t)
     in t' {variables = variables t ++ variables t', probDensity = probDensity t * probDensity t'}

density :: MonadDistribution m => Density m a -> [Double] -> m (a, [Double])
density (Density m) randomness =
  runWriterT $ evalStateT (iterTM f $ hoistFreeT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
        [] -> random
        y : ys -> put ys >> return y
      tell [x]
      k x

bernoulli :: MonadDistribution m => Double -> m Bool
bernoulli p = fmap (< p) random

discreteUniformAB :: MonadDistribution m => Int -> Int -> m Int
discreteUniformAB l u = fmap (\x -> floor (x * fromIntegral (u - l + 1)) + l) random

mhTrans :: MonadDistribution m => (Weighted (Density m)) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- density (weighted m) us'
  let ratio = min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

data Traced m a = Traced { model :: Weighted (Density Identity) a
                         , traceDist :: m (Trace a) }

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, probDensity = probDensity t1 * probDensity t2}

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, probDensity = 1}

scored :: Double -> Trace ()
scored w = Trace {variables = [], output = (), probDensity = w}

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced random (fmap singleton random)

instance MonadFactor m => MonadFactor (Traced m) where
  score' w = Traced (score' w) (score' w >> pure (scored w))

newtype SamF a = Random (Double -> a) deriving (Functor)

newtype Density m a = Density {runDensity :: FreeT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadDistribution (Density m) where
  random = Density $ liftF (Random id)

mhTrans' :: MonadDistribution m =>
             Weighted (Density Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (weightedHoist (freeHoist (return . runIdentity)) m)

weightedHoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
weightedHoist t (Weighted m) = Weighted $ mapStateT t m

freeHoist :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
freeHoist f (Density m) = Density (hoistFreeT f m)

mh :: MonadDistribution m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans' m x
        return (y :| x : xs)

geometric :: MonadDistribution m => m Int
geometric = do
  x <- random
  if x < 0.2
    then return 1
    else do y <- geometric
            return $ 1 + y

justSample :: IO [Int]
justSample = do
  setStdGen (mkStdGen 1729)
  g <- newStdGen
  stdGen <- newIOGenM g
  ds <- replicateM 1000 (uniformDouble01M stdGen)
  return $ fst $ runState (replicateM 10 $ sampleWith geometric MyGen) ds

sampleViaMh :: IO [Int]
sampleViaMh = do
  setStdGen (mkStdGen 1729)
  g <- newStdGen
  stdGen <- newIOGenM g
  ds <- replicateM 1000 (uniformDouble01M stdGen)
  return $ fst $ runState (sampleWith (unweighted $ mh 10 geometric) MyGen) ds
