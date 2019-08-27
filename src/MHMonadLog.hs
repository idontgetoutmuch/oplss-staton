{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MHMonadLog where

import Control.Monad.Trans.Writer
import Control.Monad.State
import Data.Monoid
import System.Random 
import Debug.Trace
import System.IO.Unsafe
import Control.Monad.Extra
import Numeric.Log
  
-- As programs run, they write scores (likelihoods) and we keep track of
-- the length of each run.
-- They also use randomness.
-- We consider this as a state with a stream of random numbers. 
newtype Meas a = Meas (WriterT (Product (Log Double),Sum Int) (State [Double]) a)
  deriving(Functor, Applicative, Monad)

-- Score weights the result, typically by the likelihood of an observation. 
score :: Double -> Meas ()
score r = Meas $ tell $ (Product $ (Exp . log)
                          $ (if r==0 then exp(-300) else r),Sum 0)

getrandom :: State [Double] Double
getrandom = do
  ~(r:rs) <- get
  put rs
  return r

-- Sample draws a new sample, and increments the length of the current run.
sample :: Meas Double
sample = Meas $
       do r <- lift $ getrandom
          tell $ (Product 1,Sum 1)
          return r

categ :: [Double] -> Double -> Integer
categ rs r = let helper (r':rs') r'' i =
                          if r'' < r' then i else helper rs' (r''-r') (i+1)
           in helper rs r 0

-- Output a stream of weighted samples from a program. 
weightedsamples :: Meas a -> IO [(a,Product (Log Double))]
weightedsamples (Meas m) =
                    do let helper = do
                             (x, w) <- runWriterT m
                             rest <- helper
                             return $ (x,w) : rest
                       g <- getStdGen
                       let rs = randoms g
                       let (xws,_) = runState helper rs
                       return $ map (\(x,(w,i)) -> (x,w)) xws 

-- Produce a stream of samples, together with their weights,
-- using single site Metropolis Hastings.
mh :: forall a. Meas a -> IO [(a,Product (Log Double))]
mh (Meas m) =
  do let step :: [Double] -> State [Double] [Double]
         -- Do one step of single-site Metropolis Hastings.
         -- The step involves randomness, which again we implement
         -- using the state monad. 
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
           let ratio = getProduct w' * (fromIntegral $ getSum l')
                       / (getProduct w * (fromIntegral $ getSum l))
           r'' <- getrandom
           -- probability of accepting this trace
           if r'' < (min 1 ((exp . ln) ratio)) then return as' else return as
     g <- getStdGen
     let (g1,g2) = split g
     let (samples,_) = runState (iterateM step (randoms g1)) (randoms g2)
     return $ map (\(x,(w,l)) -> (x,w)) 
       $ map (\as -> fst $ runState (runWriterT m) as)
       $ samples
     
