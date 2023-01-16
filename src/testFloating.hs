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

poisson :: (Floating a, Ord a, Num b) => a -> Meas a b
poisson lambda = do (r, _) <- f (1, 1.0)
                    return $ r - 2
  where
    bigL = exp (- lambda)
    f (k, p) = if p <= bigL
               then return (k, p)
               else do u <- sample
                       f (k + 1, p * u)

testData :: Floating a => [a]
testData = [ -0.41053319599962573
           , -0.24350213979418725
           , -0.4014408897696271
           , -0.3389331584709573
           , -0.05192727981971023
           ,  0.020827877942891426
           , -0.2415274781116573
           , -0.4446109616345316
           , -0.4181553914258611
           , -0.04247319857058479
           , -1.1553716500889604
           , -1.1409077706388022
           , -0.9644096738332144
           , -0.8596777483205384
           , -1.1281611336216917
           , -1.120624865556533
           , -1.1730038140290353
           , -1.1699325694476737
           , -0.8965705499339377
           , -0.8292623943309901
           ,  1.2635351385044336
           ,  1.2154279799451733
           ,  1.3230273322906432
           ,  1.1970918083054138
           ,  1.2379684504159412
           ,  1.2976374460867057
           ,  1.4796256370553242
           ,  1.2865851910937967
           ,  1.3270972042005351
           ,  1.382201797557261
           ]

gmm :: (RealFrac a, Floating a) => Meas a Int
gmm = do
  p <- poisson 3.0
  let bigK = 1 + floor p
  mus <- sequence $ replicate bigK $ normal' 0.0 1.0
  ws <- sequence $ replicate bigK $ normal' 0.0 1.0
  let ws' = map abs $ map (+ 0.5) ws
      ws'' = map (/ sum ws') ws'
      f = mixtureOfNormals mus (repeat 0.3) ws''
  mapM_ (score . f) testData
  return bigK
