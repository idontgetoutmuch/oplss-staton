module Regression where

import MHMonad
import Distr
import Data.Monoid
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Beta (betaDistr)


-- A random linear function
linear :: Meas (Double -> Double)
linear =
  do a <- normal 0 3
     b <- normal 0 3
     let f = \x -> a*x + b
     return f

-- Weight a function to some data points
fit :: Meas (a -> Double) -> [(a,Double)] -> Meas (a -> Double)
fit prior dataset = do f <- prior
                       mapM (\(x,y) ->
                               score $ normalPdf (f x) 0.3 y)
                            dataset
                       return f
                       
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

example :: Meas (Double -> Double)
example = fit linear dataset

-- Some graphing routines
plotWeightedPoints :: String -> [(Double,Double,Double)] -> Double -> Double -> Double -> Double -> IO ()
plotWeightedPoints filename dataset xmin xmax ymin ymax =
  let plots = map (\(x,y,w) -> toPlot $ plot_points_style .~ filledCircles 4 ((blend w blue red) `withOpacity` w)
                      $ plot_points_values .~ [(x,y)]
                      $ def) dataset in
  let my_layout = layout_plots .~ plots
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (xmin,xmax)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (ymin,ymax)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()
    
plotWeightedLines :: String -> [(Double,Double,Double)] -> IO ()
plotWeightedLines filename points =
  let plots = map (\(x,y,w) -> toPlot $ plot_lines_style . line_color .~ (blend w blue red) `withOpacity` (w)
                      $ plot_lines_values .~ [[ (0.0 :: Double,x) , (10.0 :: Double ,y*10+x) ]]
                      $ def) points in
  let my_dots = plot_points_style .~ filledCircles 4 (opaque black)
              $ plot_points_values .~ dataset
              $ def in               
  let my_layout = layout_plots .~  (toPlot my_dots : plots)
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (-10,10)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (-10,10)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()


maxlist :: [Double] -> Double
maxlist (x:xs) = max x (maxlist xs)

minlist :: [Double] -> Double
minlist (x:xs) = min x (minlist xs)


-- Plot the points drawn from weighted samples
test1 = do
        samples <- weightedsamples example
        let xyw's = map (\(f,w) -> (f 0, f 1 - f 0, w)) $ take 10000 samples
        let m = maximum (map (\(_,_,w) -> w) xyw's)
        let xyws = map (\(x,y,w) -> (x,y, max (w/m) (1/255))) xyw's
        putStr $ show $ m
        putStr $ show $ take 20 xyws
        plotWeightedPoints "weightedpoints.svg" xyws (-6) 6 (-6) 6
        --        plotWeightedLines "weightedtest.svg" xyws
        let xyw's = map (\(f,w) -> (f 0, f 1 - f 0, w)) $ take 100000 samples
        let m = maximum (map (\(_,_,w) -> w) xyw's)
        let xyws = filter (\(x,y,_) -> x > -1 && x < 0 && y > 1 && y < 2) $ map (\(x,y,w) -> (x,y, max (w / m) 0.1)) xyw's
        plotWeightedPoints "weightedpointszoom.svg" xyws (-1) 0 1 2

-- Plot the result of MH
test2 = do
        samples <- mh example
        let xyw's = map (\(f,w) -> (f 0, f 1 - f 0, getProduct w)) $ take 10000 samples
        let m = maximum (map (\(_,_,w) -> w) xyw's)
        let xyws = map (\(x,y,w) -> (x,y, max (w / m) 0.05)) xyw's
        plotWeightedPoints "weightedpointsmh.svg" xyws (-6) 6 (-6) 6
        let xyws = filter (\(x,y,_) -> x > -1 && x < 0 && y > 1 && y < 2) $ map (\(x,y,w) -> (x,y, max (w / m) 0.1)) xyw's
        plotWeightedPoints "weightedpointszoommh.svg" xyws (-1) 0 1 2


every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

-- More graphing routines
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interesting_points :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interesting_points f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon 
    then acc
    else interesting_points f lower mid epsilon delta (mid : (interesting_points f mid upper epsilon delta acc))
 
sample_fun f = 
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interesting_points f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs 

plot_funs :: String -> [(Double,Double)] -> [Double -> Double] -> IO ()
plot_funs filename dataset funs =
  let graphs  = map sample_fun funs                 in
  let my_lines  = plot_lines_style . line_color .~ blue `withOpacity` 0.01
                $ plot_lines_values .~ graphs $ def in
  let my_dots = plot_points_style .~ filledCircles 4 (opaque black)
              $ plot_points_values .~ dataset
              $ def in               
  let my_layout = layout_plots .~ [toPlot my_lines , toPlot my_dots]
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (0,6)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (-2,10)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()

-- plot the results of linear regression
test3 =
  do
    fs' <- mh $ fit linear dataset
    let fs = map (\(f,_)->f) $ take 500 $ every 100 $ drop 1000 $ fs'
    plot_funs "lin-reg.svg" dataset fs

-- plot the linear regression prior
test3b =
  do
    fs' <- mh $ linear
    let fs = map (\(f,_)->f) $ take 500 $ every 100 $ drop 1000 $ fs'
    plot_funs "lin-reg-prior.svg" dataset fs


-- poissonPP lower upper rate returns a random list of points between lower and upper coming from a Poisson process with given rate
poissonPP :: Double -> Double -> Double -> Meas [Double]
poissonPP lower upper rate =
  do gap <- exponential rate
     if (lower + gap > upper)
     then return []
     else do rest <- poissonPP (lower + gap) upper rate
             return ((lower + gap) : rest)




-- Splice takes a random list of points, a random function,
-- and returns a new random function that is a piecewise version of the
-- first one.
splice :: Meas [Double] -> Meas (Double -> Double) -> Meas (Double -> Double)
splice pointProcess randomFun =
  do xs <- pointProcess
     fs <- mapM (\_ -> randomFun) xs
     default_f <- randomFun
     let h :: [(Double, Double -> Double)] -> Double -> Double
         h [] x = default_f x
         h ((a,f):xfs) x | x <= a = f x
         h ((a,f):xfs) x | x > a = h xfs x
     return (h (zip xs fs) )

test4 =
  do
    fs' <- mh $ fit (splice (poissonPP 0 6 0.1) linear) dataset
    let fs = map (\(f,_)->f) $ take 500 $ every 1000 $ drop 100000 $ fs'
    plot_funs "piecewise-lin-reg.svg" dataset fs
