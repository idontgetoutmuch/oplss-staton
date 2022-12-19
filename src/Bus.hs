module Bus where
import MHMonad
import Distr
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.Monoid

-- Is it the weekend?
model1 = do
  -- Prior belief: it is the weekend with prob. 2/7
  x <- bernoulli (2/7)
  -- More buses on weekdays
  let rate = if x then 3 else 10
  -- observe 4 buses in an hour
  score $ poissonPdf rate 4
  return x

geometric :: Meas Int
geometric = do
  x <- sample
  if x < 0.5
    then return 1
    else do y <- geometric
            return $ 1 + y

testGeometric = do
  samples <- mh geometric
  return $ map fst $ take 10000 samples

test1 = do
  xws <- weightedsamples model1
  let xws' = map (\(x,w) -> (x, w)) xws
  _ <- renderableToFile def "bus-histogram-1a.svg"
    (chart (toHistogram (take 100000 xws)))
  return ()

model2 = do
  x <- bernoulli (2/7)
  let rate = if x then 3 else 10
  -- observe 15 mins between buses
  score $ expPdf rate 0.25
  return x

test2 = do
  xws <- weightedsamples model2
  let xws' = map (\(x,w) -> (x, w)) xws
  _ <- renderableToFile def "bus-histogram-2.svg"
    (chart (toHistogram (take 100000 xws')))
  return ()

model3 = do
  x <- bernoulli (2/7)
  let approxrate = if x then 3 else 10
-- build in an error in my estimate of the rate
  actualRate <- gamma approxrate 1
  score $ poissonPdf actualRate 4
  return x

test3 = do
  xws <- weightedsamples model3
  let xws' = map (\(x,w) -> (x, w)) xws
  _ <- renderableToFile def "bus-histogram-3.svg"
    (chart (toHistogram (take 100000 xws')))
  return ()



-- functions for plotting a histogram
totalweight :: [(a,Double)] -> Double
totalweight rxs = sum $ map (\(_,r) -> r) rxs

toHistogram :: [(Bool,Double)] -> [Double]
toHistogram rxs =
  map (\x' -> (sum $ map (\(x,r) -> if x == x' then r else 0) rxs) / (totalweight rxs)) [True,False]

chart :: [Double] -> Renderable ()
chart rs = toRenderable layout
 where
  layout =
        layout_title .~ "Histogram" ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      -- $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_y_axis . laxis_generate .~ scaledAxis def (0,1)
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double

  bars2 = plot_bars_titles .~ [""]
      $ plot_bars_values .~ addIndexes (map (\x -> [x]) rs) -- [45,30],[30,20],[70,25]]
--      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = ["weekend","weekday"]

  btitle = ""
  bstyle = Just (solidLine 1.0 $ opaque black)
  mkstyle c = (solidFillStyle c, bstyle)
