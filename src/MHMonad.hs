{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module MHMonad (
  weightedsamples,
  Meas,
  sample,
  categ,
  score,
  mh,
  test0,
  normalPdf',
  integratorStep,
  ) where

import Control.Monad.Trans.Writer
import Control.Monad.State
import Data.Monoid
import System.Random
import Control.Monad.Extra

import Numeric.AD.Internal.Forward.Double (
  ForwardDouble,
  bundle,
  primal,
  tangent,
  )

-- As programs run they write scores (likelihoods: Product Double)
-- and we keep track of the length of each run (Sum Int).
-- We also use randomness, in the form of a list of seeds [Double].
newtype Meas a = Meas (WriterT (Product Double,Sum Int) (State [Double]) a)
  deriving(Functor, Applicative, Monad)

newtype Meas0 a = Meas0 (WriterT (Product ForwardDouble, Sum Int) (State [ForwardDouble]) a)
  deriving(Functor, Applicative, Monad)

runMeas0 :: Meas0 a -> WriterT (Product ForwardDouble, Sum Int) (State [ForwardDouble]) a
runMeas0 (Meas0 m) = m

testEval0 :: Meas0 a -> [ForwardDouble] -> ((a, (Product ForwardDouble, Sum Int)), [ForwardDouble])
testEval0 x = runState (runWriterT (runMeas0 x))

normal0 :: Meas0 (ForwardDouble, ForwardDouble)
normal0 = do
   u1 <- sample0
   -- trace ("u1: " ++ show u1) $ return ()
   u2 <- sample0
   -- trace ("u2: " ++ show u2) $ return ()
   let foo = ( sqrt ((-2) * log u1) * (cos (2 * pi * u2))
             , sqrt ((-2) * log u1) * (sin (2 * pi * u2)))
   -- trace ("From normal " ++ show foo) $ return ()
   return foo

sample0 :: Meas0 ForwardDouble
sample0 = Meas0 $
       do ~(r:rs) <- get
          put rs
          tell $ (Product 1, Sum 1)
          return r

normal0' :: ForwardDouble -> ForwardDouble -> Meas0 ForwardDouble
normal0' mu sigma  = do
  x <- fst <$> normal0
  return $ sigma * x + mu

score0 :: ForwardDouble -> Meas0 ()
score0 r = Meas0 $ tell $ (Product r, Sum 0)

singleObs0 :: Meas0 ForwardDouble
singleObs0 = do
    mu <- normal0' 0.0 1.0
    score0 $ normalPdf 0.0 1.0 (bundle 2.0 1.0)
    return mu

normalPdf :: Floating a => a -> a -> a -> a
normalPdf mu sigma x =
  (recip (sqrt (2 * pi * sigma2))) * exp ((-(x - mu)^2) / (2 * sigma2))
  where
    sigma2 = sigma * sigma

normalPdf' :: Floating a => a -> a -> a -> a
normalPdf' mu sigma x = negate $ (x - mu) / sigma2 * normalPdf mu sigma x
  where
    sigma2 = sigma * sigma

test0 :: ((ForwardDouble, (Product ForwardDouble, Sum Int)), [ForwardDouble])
test0 = testEval0 singleObs0 (map (\x -> bundle x 0.0) [0.5, 0.5])

-- Score weights the result, typically by the likelihood of an observation.
score :: Double -> Meas ()
score r = Meas $ tell $ (Product r,Sum 0)

-- Sample draws a new sample. We first use the given deterministic bits,
-- and then move to the stream of randoms when that is used up.
-- We keep track of the numbers used in any given run.
sample :: Meas Double
sample = Meas $
       do ~(r:rs) <- get
          put rs
          tell $ (Product 1,Sum 1)
          return r

categ :: [Double] -> Double -> Integer
categ rs r = let helper (r':rs') r'' i =
                          if r'' < r' then i else helper rs' (r''-r') (i+1)
                 helper _ _ _ = error "categ"
           in helper rs r 0

-- Output a stream of weighted samples from a program.
weightedsamples :: forall a. Meas a -> IO [(a,Double)]
weightedsamples (Meas m) =
                    do let helper :: State [Double]
                                     [(a,(Product Double,Sum Int))]
                           helper = do
                             (x, w) <- runWriterT m
                             rest <- helper
                             return $ (x,w) : rest
                       g <- getStdGen
                       let rs = randoms g
                       let (xws,_) = runState helper rs
                       return $ map (\(x, (w, _)) -> (x, getProduct w)) xws

getrandom :: State [Double] Double
getrandom = do
  ~(r:rs) <- get
  put rs
  return r

-- Produce a stream of samples, together with their weights,
-- using single site Metropolis Hastings.
mh :: forall a. Meas a -> IO [(a,Product Double)]
mh (Meas m) =
  do -- helper takes a random source and the previous result
     -- and produces a stream of result/weight pairs
     let step :: [Double] -> State [Double] [Double]
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
     g <- getStdGen
     let (g1,g2) = split g
     let (samples,_) = runState (iterateM step (randoms g1)) (randoms g2)
     return $ map (\(x, (w, _)) -> (x, w))
       $ map (\as -> fst $ runState (runWriterT m) as)
       $ samples

leapFrogSteps :: Int
leapFrogSteps = 25

data PhaseState a = PhaseState { phaseState :: [(a, a, Bool)] }
  deriving (Eq, Show)

getQs :: PhaseState a -> [a]
getQs = map (\(q, _, _) -> q) . phaseState

getPs :: PhaseState a -> [a]
getPs = map (\(_, p, _) -> p) . phaseState

getIsConts :: PhaseState a -> [Bool]
getIsConts =  map (\(_, _, c) -> c) . phaseState

-- | Performs one integrator step (called "leapfrog step" in standard HMC).
integratorStep :: Meas0 a -> Double -> PhaseState ForwardDouble -> (ForwardDouble, PhaseState Double)
integratorStep (Meas0 m) eps as = (resultNew', PhaseState $ zip3 stateQsNew' statePsNew' (getIsConts as))
  where
    statePs = getPs as
    stateQs = getQs as
    -- FIXME: For compatibility with the python for now
    stateIsConts = map (fromIntegral . fromEnum) $ getIsConts as
    result = getProduct $ fst $ snd $ fst $ runState (runWriterT m) stateQs
    gradU = tangent result
    statePsNew = zipWith (\stateP stateIsCont -> primal stateP - eps / 2 * gradU * stateIsCont)
                         statePs stateIsConts
    stateQsNew = zipWith3 (\stateQ stateP stateIsCont  -> primal stateQ + eps / 2 * stateP * stateIsCont)
                          stateQs statePsNew stateIsConts
    -- FIXME: For now ignore discontinuous coordinates
    _resultNew = getProduct $ fst $ snd $ fst $ runState (runWriterT m) (map (\x -> bundle x 1.0) stateQsNew)
    --
    -- # Integrate the discontinuous coordinates in a random order:
    -- disc_indices = torch.flatten(torch.nonzero(~state.is_cont, as_tuple=False))
    -- perm = torch.randperm(len(disc_indices))
    -- disc_indices_permuted = disc_indices[perm]
    -- for j in disc_indices_permuted:
    --     if j >= len(state.q):
    --         continue  # out-of-bounds can happen if q changes length during the loop
    --     result = coord_integrator(
    --         run_prog, int(j.item()), t, eps, state, state_0, result
    --     )
    --
    -- Second half of leapfrog step for continuous variables
    stateQsNew' = zipWith3 (\stateQ stateP stateIsCont -> stateQ + eps / 2 * stateP * stateIsCont)
                           stateQsNew statePsNew stateIsConts
    resultNew' = getProduct $ fst $ snd $ fst $ runState (runWriterT m) (map (\x -> bundle x 1.0) stateQsNew')
    gradUNew = tangent resultNew'
    statePsNew' = zipWith (\stateP stateIsCont -> stateP - eps/ 2 * gradUNew * stateIsCont) statePsNew stateIsConts


