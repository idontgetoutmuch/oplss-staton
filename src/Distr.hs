module Distr where

import MHMonad
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import qualified Statistics.Distribution.Poisson as Poisson
import Data.List

categorical :: [Double] -> Meas Integer
categorical rs = do r <- sample
                    return $ categ rs r


normal :: Double -> Double -> Meas Double
normal m s = do x <- sample
                return $ quantile (normalDistr m s) x

normalPdf :: Double -> Double -> Double -> Double
normalPdf m s = density $ normalDistr m s

exponential :: Double -> Meas Double
exponential rate =
  do x <- sample
     return $ - (log x / rate)

expPdf :: Double -> Double -> Double
expPdf rate x = exp (-rate*x) * rate

gamma :: Double -> Double -> Meas Double
gamma a b = do
  x <- sample
  return $ quantile (gammaDistr a b) x

beta :: Double -> Double -> Meas Double
beta a b = do
  x <- sample
  return $ quantile (betaDistr a b) x

poisson :: Double -> Meas Integer
poisson lambda = do
  x <- sample
  let cmf = scanl1 (+) $ map (probability $ Poisson.poisson lambda) $ [0,1..]
  let (Just n) = findIndex (\r -> r > x) cmf
  return $ fromIntegral n

poissonPdf :: Double -> Integer -> Double
poissonPdf rate n = probability (Poisson.poisson rate) (fromIntegral n)

dirichlet :: [Double] -> Meas[Double]
dirichlet as = do
  xs <- mapM exponential as
  let s = Prelude.sum xs
  let ys = map (/ s) xs
  return ys

bernoulli :: Double -> Meas Bool
bernoulli r = do
  x <- sample
  return $ x < r
