{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module MHMonad (
  weightedsamples,
  Meas,
  sample,
  categ,
  score,
  mh,
  testEval,
  testGrad,
  ) where

import Control.Monad.Trans.Writer
import Control.Monad.State
import Data.Monoid
import System.Random
import Control.Monad.Extra
import Numeric.Backprop
import Numeric.AD.Newton
import Numeric.AD.Internal.Forward
import Numeric.AD.Internal.Type


-- As programs run they write scores (likelihoods: Product Double)
-- and we keep track of the length of each run (Sum Int).
-- We also use randomness, in the form of a list of seeds [Double].
newtype Meas b a = Meas (WriterT (Product b, Sum Int) (State [b]) a)
  deriving(Functor, Applicative, Monad)

myFmap :: (b -> c) -> Meas b a -> Meas c a
myFmap f (Meas m) = undefined

moof :: ((c, (Product e, Sum Int)) -> (d, (Product f, Sum Int))) ->
        WriterT (Product e, Sum Int) (State [b]) c ->
        WriterT (Product f, Sum Int) (State [b]) d
moof f m = mapWriterT (fmap f) m

moog :: (e -> f) ->
        WriterT (Product e, Sum Int) (State [b]) c ->
        WriterT (Product f, Sum Int) (State [b]) c
moog f = moof (\(x, (y, z)) -> (x, (fmap f y, z)))

newtype Meat c b a = Meat (WriterT (Product b, Sum Int) (State [c]) a)
  deriving(Functor, Applicative, Monad)

foof :: ((c, (Product e, Sum Int)) -> (d, (Product f, Sum Int))) ->
        Meat b e c -> -- WriterT (Product e, Sum Int) (State [b]) c ->
        Meat b f d -- WriterT (Product f, Sum Int) (State [b]) d
foof f (Meat m) = Meat $ mapWriterT (fmap f) m

foog :: (e -> f) ->
        Meat b e c ->
        Meat b f c
foog f = foof (\(x, (y, z)) -> (x, (fmap f y, z)))


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

sampleC :: (Num a, Num b) => Meat a b a
sampleC = Meat $
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

leapFrogSteps :: Int
leapFrogSteps = 25

-- def integrator_step(
--     run_prog: Callable[[torch.Tensor], ProbRun[T]],
--     t: float,
--     eps: float,
--     state: State,
--     state_0: State,
-- ) -> ProbRun[T]:
--     """Performs one integrator step (called "leapfrog step" in standard HMC)."""
--     result = run_prog(state.q)
--     # first half of leapfrog step for continuous variables:
--     state.p = state.p - eps / 2 * result.gradU() * state.is_cont
--     state.q = state.q + eps / 2 * state.p * state.is_cont
--     result = run_prog(state.q)
--     # Integrate the discontinuous coordinates in a random order:
--     disc_indices = torch.flatten(torch.nonzero(~state.is_cont, as_tuple=False))
--     perm = torch.randperm(len(disc_indices))
--     disc_indices_permuted = disc_indices[perm]
--     for j in disc_indices_permuted:
--         if j >= len(state.q):
--             continue  # out-of-bounds can happen if q changes length during the loop
--         result = coord_integrator(
--             run_prog, int(j.item()), t, eps, state, state_0, result
--         )
--     # second half of leapfrog step for continuous variables
--     state.q = state.q + eps / 2 * state.p * state.is_cont
--     result = run_prog(state.q)
--     state.p = state.p - eps / 2 * result.gradU() * state.is_cont
--     return result

data PhaseState a = PhaseState { phaseState :: [(a, a, Bool)] }
  deriving (Eq, Show)

-- integratorStep :: Meas b a -> [b] -> Double -> Double -> PhaseState b -> PhaseState b -> IO c
-- integratorStep :: Meas a1 a2 -> [a1] -> p1 -> p2 -> p3 -> p4 -> a
-- integratorStep m as t eps phaseState phastState0 = undefined
--   where
--     l :: _
--     l = evalBP (foo m . sequenceVar) undefined
--     k = foo m as

-- Produce a stream of samples, together with their weights,
-- using single site Metropolis Hastings.
hmc :: forall a b . (Num b, Random b, Ord b, Fractional b) => Meas b a -> IO [(a, Product b)]
hmc (Meas m) =
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

foo1 :: Backprop b => Meas b a -> (forall s . Reifies s W => BVar s [b] -> BVar s b)
foo1 (Meas m) = (getProduct . fst . snd . fst . runState (runWriterT ((let Meas n = myFmap auto (Meas m) in n)))) . sequenceVar

foo3 :: (Backprop a1, Reifies s1 W) =>
        WriterT (Product e, Sum Int) (State [BVar s1 a1]) a2 ->
        BVar s1 [a1] ->
        BVar s2 e
foo3 m = (getProduct . fst . snd . fst . runState (runWriterT (moog auto m))) . sequenceVar

-- foo2 :: Backprop b => Meat c b a -> (forall s . Reifies s W => BVar s [b] -> BVar s b)
foo2 (Meat m) = (getProduct . fst . snd . fst . runState (runWriterT ((let Meat n = foog auto (Meat m) in n)))) . sequenceVar

foo4 :: Reifies s W => Backprop b => Meas (BVar s b) a -> (forall s . Reifies s W => BVar s [b] -> BVar s b)
foo4 (Meas m) = undefined -- (getProduct . fst . snd . fst . runState (runWriterT m)) . sequenceVar


-- | Performs one integrator step (called "leapfrog step" in standard HMC).
--
-- integratorStep :: (Reifies s W, Backprop b) => d ~ BVar s b => Meas b a -> [b] -> Double -> Double -> PhaseState b -> PhaseState b -> IO c
-- integratorStep :: forall s b a p p1 p2 p3 a1 . (Reifies s W, Backprop b) =>
--                   Meas (BVar s b) a -> [BVar s b] -> p -> p1 -> p2 -> p3 -> a1
integratorStep :: Reifies s W => Backprop b => Meas (BVar s b) a -> Int
integratorStep m {- as t eps phaseState phastState0 -} = undefined
  where
    -- result = run_prog(state.q)
    -- k :: BVar s b
    -- k = foo m as
    -- l :: forall s . BVar s [b] -> BVar s b
    -- l = foo m . sequenceVar
    -- j :: [b] -> [b]
    -- j = gradBP (foo1 m)
    -- k = gradBP (foo2 m)
    l = gradBP (foo4 m)
    -- first half of leapfrog step for continuous variables:
    -- state.p = state.p - eps / 2 * result.gradU() * state.is_cont
    -- state.p = state.p - eps / 2 * result.gradU() * state.is_cont

--     state.q = state.q + eps / 2 * state.p * state.is_cont
--     result = run_prog(state.q)
--     # Integrate the discontinuous coordinates in a random order:
--     disc_indices = torch.flatten(torch.nonzero(~state.is_cont, as_tuple=False))
--     perm = torch.randperm(len(disc_indices))
--     disc_indices_permuted = disc_indices[perm]
--     for j in disc_indices_permuted:
--         if j >= len(state.q):
--             continue  # out-of-bounds can happen if q changes length during the loop
--         result = coord_integrator(
--             run_prog, int(j.item()), t, eps, state, state_0, result
--         )
--     # second half of leapfrog step for continuous variables
--     state.q = state.q + eps / 2 * state.p * state.is_cont
--     result = run_prog(state.q)
--     state.p = state.p - eps / 2 * result.gradU() * state.is_cont
--     return result

foo :: Reifies s W => Meas (BVar s b) a -> [BVar s b] -> BVar s b
foo (Meas m) as = getProduct $ fst $ snd $ fst $ runState (runWriterT m) as

bar :: Double
bar = evalBP (foo singleObs . sequenceVar) [0.5]

testEval :: Bool
testEval = bar == normalPdf 0.0 1.0 (0.0 :: Double)

baz :: [Double]
baz = gradBP (foo singleObs . sequenceVar) [0.5]

testGrad :: Bool
testGrad = baz == [0.0 :: Double]

singleObs :: Floating a => Meas a a
singleObs = do
    mu <- normal' 0.0 1.0
    -- trace (show mu) $ return ()
    score $ normalPdf 0.0 1.0 0.0
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
