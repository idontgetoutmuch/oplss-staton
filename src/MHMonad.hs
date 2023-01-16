{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module MHMonad (
  weightedsamples,
  Meas,
  sample,
  categ,
  score,
  mh,
  ) where

import Control.Monad.Trans.Writer
import Control.Monad.State
import Data.Monoid
import System.Random
import Control.Monad.Extra
import Numeric.Backprop

-- As programs run they write scores (likelihoods: Product Double)
-- and we keep track of the length of each run (Sum Int).
-- We also use randomness, in the form of a list of seeds [Double].
newtype Meas b a = Meas (WriterT (Product b, Sum Int) (State [b]) a)
  deriving(Functor, Applicative, Monad)

-- Score weights the result, typically by the likelihood of an observation.
score :: a -> Meas a ()
score r = Meas $ tell $ (Product r, Sum 0)

-- Sample draws a new sample. We first use the given deterministic bits,
-- and then move to the stream of randoms when that is used up.
-- We keep track of the numbers used in any given run.
sample :: Num a => Meas a a
sample = Meas $
       do ~(r:rs) <- get
          put rs
          tell $ (Product 1, Sum 1)
          return r

categ :: (Ord a, Num a) => [a] -> a -> Integer
categ rs r = let helper (r':rs') r'' i =
                          if r'' < r' then i else helper rs' (r''-r') (i+1)
                 helper _ _ _ = error "categ"
           in helper rs r 0

-- Output a stream of weighted samples from a program.
weightedsamples :: forall a b . (Random b) => Meas b a -> IO [(a, b)]
weightedsamples (Meas m) =
                    do let helper :: State [b]
                                     [(a,(Product b, Sum Int))]
                           helper = do
                             (x, w) <- runWriterT m
                             rest <- helper
                             return $ (x,w) : rest
                       g <- getStdGen
                       let rs = randoms g
                       let (xws,_) = runState helper rs
                       return $ map (\(x, (w, _)) -> (x, getProduct w)) xws

getrandom :: State [b] b
getrandom = do
  ~(r:rs) <- get
  put rs
  return r

-- Produce a stream of samples, together with their weights,
-- using single site Metropolis Hastings.
mh :: forall a b . (Num b, Random b, Ord b, Fractional b) => Meas b a -> IO [(a, Product b)]
mh (Meas m) =
  do -- helper takes a random source and the previous result
     -- and produces a stream of result/weight pairs
     let step :: [b] -> State [b] [b]
         -- each step will use three bits of randomness,
         -- plus any extra randomness needed when rerunning the model
         step as = do
           let ((_, (w,l)),_) =
                 runState (runWriterT m) as
           -- randomly pick which site to change
           r <- getrandom
           let i = categ (replicate (fromIntegral $ getSum l)
                          (1/(fromIntegral $ getSum l))) r
           -- replace that site with a new random choice
           r' <- getrandom
           let as' =
                 (let (as1,_:as2) = splitAt (fromIntegral i) as in
                    as1 ++ r' : as2)
           -- rerun the model with the original and changed sites
           let ((_, (w',l')),_) =
                 runState (runWriterT m) (as')
           -- calculate the acceptance ratio
           let ratio = getProduct w' * (fromIntegral $ getSum l)
                       / (getProduct w * (fromIntegral $ getSum l'))
           r'' <- getrandom
           -- probability of accepting this trace
           if r'' < (min 1 ratio) then return as' else return as
     setStdGen (mkStdGen 42)
     g <- getStdGen
     let (g1,g2) = split g
     let (samples,_) = runState (iterateM step (randoms g1)) (randoms g2)
     return $ map (\(x, (w, _)) -> (x, w))
       $ map (\as -> fst $ runState (runWriterT m) as)
       $ samples

-- foo :: Reifies s W => Meas (BVar s b) a -> [BVar s b] -> BVar s b
foo (Meas m) as = getProduct $ fst $ snd $ fst $ runState (runWriterT m) as

singleObs :: Floating a => Meas a a
singleObs = do
    mu <- fst <$> normal
    score $ normalPdf mu 1.0 4.0
    return mu

normal :: Floating a => Meas a (a, a)
normal = do
   u1 <- sample
   u2 <- sample
   return ( sqrt ((-2) * log u1) * (cos (2 * pi * u2))
          , sqrt ((-2) * log u1) * (sin (2 * pi * u2)))

normal' :: Floating a => a -> a -> Meas a a
normal' mu sigma  = do
  x <- fst <$> normal
  return $ sigma * x + mu

normalPdf :: Floating a => a -> a -> a -> a
normalPdf mu sigma x =
  (recip (sqrt (2 * pi * sigma2))) * exp ((-(x - mu)^2) / (2 * sigma2))
  where
    sigma2 = sigma * sigma
