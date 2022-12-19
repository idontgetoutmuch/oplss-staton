module Proginduction where

import MHMonad
import Distr
import Data.Monoid
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens

-- A grammar for a simple expression language
data BinOp = Add | Multiply
data UnOp = Neg
data Expr = BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | Number Double
          | Var
          | Cond BoolExpr Expr Expr
data BoolExpr = Leq Expr Expr

-- Simple evaluation function for expressions
eval :: Expr -> Double -> Double
eval (BinOp Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (BinOp Multiply e1 e2) x = (eval e1 x) * (eval e2 x)
eval (UnOp Neg e ) x = - (eval e x)
eval (Number r) x = r
eval (Cond (Leq e1 e2) e3 e4) x = if (eval e1 x) <= (eval e2 x) then (eval e3 x) else (eval e4 x)
eval Var x = x

-- a probabilistic context free grammar
pcfgbinop :: Meas BinOp
pcfgbinop = do n <- categorical [0.5,0.5]
               case n of 0 -> return Add
                         1 -> return Multiply

pcfgexpr :: Meas Expr
pcfgexpr = do n <- categorical [0.2,0.1,0.3,0.39,0.01]
              case n of 0 -> do {o <- pcfgbinop ; e1 <- pcfgexpr ; e2 <- pcfgexpr ; return $ BinOp o e1 e2 }
                        1 -> do {e <- pcfgexpr ; return $ UnOp Neg e }
                        2 -> do {r <- normal 0 3 ; return $ Number r }
                        3 -> return $ Var
                        4 -> do {e1 <- return Var ; r <- normal 0 4 ; e3 <- pcfgexpr ; e4 <- pcfgexpr ; return $ Cond (Leq e1 (Number r)) e3 e4 }

-- length of an expression
len :: Expr -> Integer
len (BinOp _ e1 e2) = len e1 + len e2 + 1
len (UnOp _ e1) = len e1 + 1
len (Number r) = 1
len Var = 1
len (Cond (Leq e1 e2) e3 e4) = len e1 + len e2 + len e3 + len e4 + 1

-- a random expression is drawn from the context free grammar,
-- but then we weight to get a more reasonable length
randexpr :: Meas (Double -> Double)
randexpr = do { e <- pcfgexpr ;
                score ((normalPdf 25 5) (fromIntegral $ len e)) ;
                return $ \x -> eval e x }


-- functions for plotting a histogram
totalweight :: [(Integer,Double)] -> Double
totalweight rxs = sum $ map (\(_,r) -> r) rxs

toHistogram :: [(Integer,Double)] -> Integer -> Integer -> [Double]
toHistogram rxs from to = if from > to then [] else
  ((sum $ map (\(x,r) -> if x == from then r else 0) rxs) / (totalweight rxs)) : (toHistogram rxs (from + 1) to)

chart :: [Double] -> Renderable ()
chart rs = toRenderable layout
 where
  layout =
        layout_title .~ "Sample Bars" ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double

  bars2 = plot_bars_titles .~ [""]
      $ plot_bars_values .~ addIndexes (map (\x -> [x]) rs) -- [45,30],[30,20],[70,25]]
--      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = map show [0 .. ((length rs)-1)]

  btitle = ""
  bstyle = Just (solidLine 1.0 $ opaque black)
  mkstyle c = (solidFillStyle c, bstyle)

-- plot a histogram of lengths when the weighted pcfg grammar is used
test1 =
  let test =
        do
          e <- pcfgexpr
          -- score ((density $ normalDistr 25 5) (fromIntegral $ len e))
          return $ len e
  in
  do
    rxs' <- weightedsamples test
    let rxs = map (\(x,r) -> (x,r)) rxs'
    _ <- renderableToFile def "histogram.svg" (chart (toHistogram (take 100000 rxs) 0 40))
    return ()

sample_fun f =
  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]

plot_funs :: String -> [(Double,Double)] -> [Double -> Double] -> IO ()
plot_funs filename dataset funs =
  let graphs  = map sample_fun funs                 in
  let my_lines  = plot_lines_style . line_color .~ blue `withOpacity` 0.1
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


dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

fit prior dataset = do f <- prior
                       mapM (\(x,y) -> score $ (density $ normalDistr (f x) 0.3) y) dataset
                       return f

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

-- Plot some fitted expressions
test3 =
  do
    fs' <- mh $ fit randexpr dataset
    let fs = map (\(f,_)->f) $ take 10 $ every 1000 $ drop 1000000 $ fs'
    plot_funs "prog-ind.svg" dataset fs
    return ()

-- Histogram of the posterior length
test4 =
  do
    let posteriorLength =
          do e <- pcfgexpr
             mapM (\(x,y) -> score $ (density $ normalDistr (eval e x) 0.3) y) dataset
             return (len e)
    ls <- mh $ posteriorLength
    let rxs = map (\(x,r) -> (x,getProduct r)) ls
    _ <- renderableToFile def "histogram.svg" (chart (toHistogram (take 100000 $ drop 100000 rxs) 0 40))
    return ()
