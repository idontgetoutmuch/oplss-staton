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

module MHMonadSystemRandom where

import Control.Monad.Writer
import Control.Monad.State
import System.Random hiding (random)
import Control.Monad.Extra hiding (bind)

import System.Random.Stateful hiding (random)
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Applicative
import Data.Word
import Data.Bits
import Data.Ratio
import Control.Monad.Trans.Free
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

import RandomNumbersFromPython

getrandom' :: StatefulGen r m => ReaderT r m Double
getrandom' = ReaderT uniformDouble01M

-- -- sample :: MeasR Double
-- sample' = -- MeasR $
--         do tell $ (Product 1,Sum 1)
--            undefined -- getrandom'

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

rollM :: StatefulGen g m => g -> m Word
rollM = uniformRM (1, 6)

pureGen :: StdGen
pureGen = mkStdGen 42

testDoesRep :: [Word]
testDoesRep = runStateGen_ pureGen (replicateM 10 . rollM)

testDoesRepB :: [Word8]
testDoesRepB = evalState (replicateM 10 $ uniformWord8 MyGen) rnfp

testDoesRepC :: [Double]
testDoesRepC = evalState (replicateM 10 $ uniformDouble01M MyGen) rnfp

-- | Monads that can draw random variables.
class Monad m => MonadDistribution m where
  -- | Draw from a uniform distribution.
  random ::
    -- | \(\sim \mathcal{U}(0, 1)\)
    m Double

normal :: MonadDistribution m => m (Double, Double)
normal = do
   u1 <- random
   u2 <- random
   return ( sqrt ((-2) * log u1) * (cos (2 * pi * u2))
          , sqrt ((-2) * log u1) * (sin (2 * pi * u2)))

normal' :: MonadDistribution m => Double -> Double -> m Double
normal' mu sigma  = do
  x <- fst <$> normal
  return $ sigma * x + mu

normalPdf :: Double -> Double -> Double -> Double
normalPdf mu sigma x =
  (recip (sqrt (2 * pi * sigma2))) * exp ((-(x - mu)^2) / (2 * sigma2))
  where
    sigma2 = sigma * sigma

class Monad m => MonadFactor m where
  score' :: Double -> m ()

newtype Sampler g m a = Sampler (ReaderT g m a) deriving (Functor, Applicative, Monad)

instance StatefulGen g m => MonadDistribution (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

sampleWith :: StatefulGen g m => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

testPseudoMonadBayes :: Double
testPseudoMonadBayes = evalState (sampleWith random MyGen) rnfp

testNormalMean :: Double
testNormalMean = sum $
                 evalState (sampleWith (replicateM 100 $ normal' 0.0 1.0) MyGen) rnfp

testNormalVar :: Double
testNormalVar = sum $ map (\x -> x * x) $
                evalState (sampleWith (replicateM 100 $ normal' 0.0 1.0) MyGen) rnfp

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  random = lift random

newtype Weighted m a = Weighted (StateT Double m a)
  -- StateT is more efficient than WriterT
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadDistribution)

instance Monad m => MonadFactor (Weighted m) where
  score' w = Weighted (modify (* w))

class (MonadDistribution m, MonadFactor m) => MonadMeasure m

singleObs :: MonadDistribution m => Weighted m Double
singleObs = do
    mu <- normal' 0.0 1.0
    score' $ normalPdf mu 1.0 4.0
    return mu

weighted :: Weighted m a -> m (a, Double)
weighted (Weighted m) = runStateT m 1

unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

testSingleObs :: (Double, Double)
testSingleObs = evalState ((flip sampleWith MyGen . weighted) singleObs) rnfp

-- | Collection of random variables sampler during the program's execution.
data Trace a = Trace
  { -- | Sequence of random variables sampler during the program's execution.
    variables :: [Double],
    --
    output :: a,
    -- | The probability of observing this particular sequence.
    probDensity :: Double
  }

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

mhTrans :: (MonadDistribution m, StatefulGen g m) =>
           g -> Weighted (Density m) a -> Trace a -> m (Trace a)
mhTrans g m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- uniformRM (0, n - 1) g
    u' <- uniformDouble01M g
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- density (weighted m) us'
  let ratio = min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  r <- uniformDouble01M g
  let accept = r < ratio
  return $ if accept then Trace vs b q else t

testOneStep  :: (StatefulGen MyGen m, MonadDistribution m) =>
                Trace Double -> m (Trace Double)
testOneStep = mhTrans MyGen singleObs

mh' :: (MonadDistribution m, StatefulGen g m) =>
       g -> Int -> Weighted (Density m) a -> Trace a -> m [Trace a]
mh' g n m t = unfoldM f n
  where
    f 0 = return Nothing
    f k = do u <- mhTrans g m t
             return $ Just (u, k - 1)

testMh' :: (StatefulGen MyGen m, MonadDistribution m) => m [Trace Double]
testMh' = mh' MyGen 0 singleObs Trace {variables = [0.5], output = 0.5, probDensity = 1}

-- | Tracing monad that records random choices made in the program.
data Traced m a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (Density Identity) a,
    -- | Record trace and output.
    traceDist :: m (Trace a)
  }

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

-- instance MonadMeasure m => MonadMeasure (Traced m)

newtype SamF a = Random (Double -> a) deriving (Functor)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype Density m a = Density {runDensity :: FreeT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- instance MonadFree SamF (Density m) where
--   wrap = Density . wrap . fmap runDensity

instance Monad m => MonadDistribution (Density m) where
  random = Density $ liftF (Random id)

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: (StatefulGen g m, MonadDistribution m) =>
            g -> Weighted (Density Identity) a -> Trace a -> m (Trace a)
mhTrans' g m = mhTrans g (weightedHoist (freeHoist (return . runIdentity)) m)

weightedHoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
weightedHoist t (Weighted m) = Weighted $ mapStateT t m

freeHoist :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
freeHoist f (Density m) = Density (hoistFreeT f m)

mh :: (StatefulGen g m, MonadDistribution m) =>
      g -> Int -> Traced m a -> m [a]
mh g n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans' g m x
        return (y :| x : xs)

geometric :: MonadDistribution m => m Int
geometric = do
  x <- random
  if x < 0.2
    then return 1
    else do y <- geometric
            return $ 1 + y

whatever = runIdentity (runStateT (sampleWith geometric MyGen) rnfp :: Identity (Int, [Double]))

mhRunGeometric :: IO [Int]
mhRunGeometric = do
  setStdGen (mkStdGen 43)
  g <- newStdGen
  let (g1, g2) = split g
  stdGen1 <- newIOGenM g1
  stdGen2 <- newIOGenM g2
  undefined
  -- runReaderT (unweighted $ (runReaderT (mh 1000 geometric) stdGen1)) stdGen2
