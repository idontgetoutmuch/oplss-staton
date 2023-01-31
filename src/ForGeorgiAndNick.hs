{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveFunctor        #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import           Numeric.Backprop
import           Control.Monad.Trans.Writer
import           Control.Monad.State
import           Data.Monoid

-- Here's the original type. We want to be able to not just get hold
-- of `Product b` but we want to get its derivative with respect to `State [b]`.
newtype Meas b a = Meas (WriterT (Product b, Sum Int) (State [b]) a)
  deriving(Functor, Applicative, Monad)

-- Unwrapping the above defintion and replacing `State [b]` with `BVar s [b]` we have
type Meas1  b a = forall s . Reifies s W => BVar s [b] -> ((a, (Product (BVar s b), Sum Int)), BVar s [b])

-- Unwrapping a bit more, we can create something of the correct type for `evalBP` and `gradBP`
unwrap :: forall a b s . (Backprop a, Backprop b, Reifies s W) =>
     Meas1 b a -> (BVar s [b] -> BVar s ((a, (Product b, Sum Int)), [b]))
unwrap phi = \xs -> let ys = snd $ phi xs
                        u = auto $ fst $ fst $ phi xs
                        v :: Product (BVar s b)
                        v = fst $ snd $ fst $ phi xs
                        v' :: BVar s (Product b)
                        v' = collectVar v
                        w = auto $ snd $ snd $ fst $ phi xs
                    in T2 (T2 u (T2 v' w)) ys

-- So here is my attempt at a replacement for `Meas`
newtype Meas2 b a = Meas2 (forall s . Reifies s W => WriterT (Product (BVar s b), Sum Int) (State (BVar s [b])) a)
  deriving Functor

runMeas2 :: Reifies s W => Meas2 b a -> WriterT (Product (BVar s b), Sum Int) (State (BVar s [b])) a
runMeas2 (Meas2 m) = m

-- GHC won't derive the instances automatically
instance Num b => Applicative (Meas2 b) where
  pure = return
  (<*>) = ap

-- GHC won't derive the instances automatically
instance Num b => Monad (Meas2 b) where
  return x = Meas2 (return x)
  (Meas2 x) >>= f = Meas2 (x >>= runMeas2 . f)

-- So now we can evaluate the type
testEval2 :: (Backprop a, Backprop b) => Meas2 b a -> [b] -> ((a, (Product b, Sum Int)), [b])
testEval2 x = evalBP (unwrap (runState (runWriterT (runMeas2 x))))

-- And we can evaluate the derivative of the type
testGrad2 :: (Backprop a, Backprop b) => Meas2 b a -> [b] -> [b]
testGrad2 x = gradBP (unwrap (runState (runWriterT (runMeas2 x))))

-- In our original library we can take samples
sample :: Num a => Meas a a
sample = Meas $
       do ~(r:rs) <- get
          put rs
          tell $ (Product 1, Sum 1)
          return r

-- But now I am stuck
sample2 :: (Backprop b, Num b, Reifies s W) =>
           WriterT (Product (BVar s b), Sum Int) (State (BVar s [b])) (BVar s b)
sample2 = -- Meas2 $
       do ~(r:rs) <- sequenceVar <$> get
          tell $ (Product 1, Sum 1)
          return r

-- As Nick and Georgi suggest, I have added the `s` as a type variable
-- and removed the constraint
newtype Meas3 s b a = Meas3 (WriterT (Product (BVar s b), Sum Int) (State (BVar s [b])) a)
  deriving Functor

runMeas3 :: Reifies s W => Meas3 s b a -> WriterT (Product (BVar s b), Sum Int) (State (BVar s [b])) a
runMeas3 (Meas3 m) = m

-- GHC still won't derive the instances automatically
instance (Reifies s W, Num b) => Applicative (Meas3 s b) where
  pure = return
  (<*>) = ap

-- GHC still won't derive the instances automatically
instance (Reifies s W, Num b) => Monad (Meas3 s b) where
  return x = Meas3 (return x)
  (Meas3 x) >>= f = Meas3 (x >>= runMeas3 . f)

testEvalPre3 :: Reifies s W =>
                Meas3 s b a -> BVar s [b] -> ((a, (Product (BVar s b), Sum Int)), BVar s [b])
testEvalPre3 x = runState (runWriterT (runMeas3 x))

-- But now I can't get `Meas3` into the form that can be fed into `evalBP` or `gradBP`
testEval3 :: (Backprop a, Backprop b) => Meas3 s b a -> [b] -> ((a, (Product b, Sum Int)), [b])
testEval3 x = undefined
              -- evalBP (unwrap (runState (runWriterT (runMeas3 x))))

-- normal :: Floating a => Meas4 a (a, a)
-- normal = do
--    u1 <- sample
--    u2 <- sample
--    return ( sqrt ((-2) * log u1) * (cos (2 * pi * u2))
--           , sqrt ((-2) * log u1) * (sin (2 * pi * u2)))

-- normal' :: Floating a => a -> a -> Meas4 a a
-- normal' mu sigma  = do
--   x <- fst <$> normal
--   return $ sigma * x + mu

normalPdf :: Floating a => a -> a -> a -> a
normalPdf mu sigma x =
  (recip (sqrt (2 * pi * sigma2))) * exp ((-(x - mu)^2) / (2 * sigma2))
  where
    sigma2 = sigma * sigma
