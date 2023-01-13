module TestFloating where

import MHMonad
import Numeric.AD

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

mixtureOfNormals :: Floating a => [a] -> [a] -> [a] -> a -> a
mixtureOfNormals mus vars ws x = sum $ zipWith (*) cs ws
  where
    cs = zipWith (\mu var -> normalPdf mu (sqrt var) x) mus vars

gmm :: (RealFrac a, Floating a) => Meas a Int
gmm = do
  p <- normal' 3.0 1.0
  let bigK = 1 + floor (abs p)
  mus <- sequence $ replicate bigK $ normal' 0.0 1.0
  ws <- sequence $ replicate bigK $ normal' 0.0 1.0
  let ws' = map abs $ map (+ 0.5) ws
      ws'' = map (/ sum ws') ws'
      f = mixtureOfNormals mus (repeat 0.3) ws''
  mapM_ (score . f) [undefined] -- testData
  return bigK
