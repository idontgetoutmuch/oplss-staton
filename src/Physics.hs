{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Physics where
import           Apecs.Physics
import           Apecs.Physics.Gloss
import           Control.Monad
import           Graphics.Gloss.Interface.IO.Simulate
import           Graphics.Gloss
import Control.Concurrent
import Data.IORef
import MHMonadLog
import DistrLog
import System.IO.Unsafe
import Debug.Trace
import Data.List

{-- A simple 2d physics problem based on the Chipmunk / Apecs library
    Inspired by a tutorial example in Anglican by Frank Wood and others --}
makeWorld "World" [''Physics, ''Camera]


initialize bumpers = do
  set global ( Camera (V2 0 1) 60
             , earthGravity )

  -- The bumpers
  mapM (\(x,y,theta) -> do
          lineBody <- newEntity (StaticBody, Angle (theta), Position (V2 x y))
          newEntity (Shape lineBody (hLine 2), Elasticity 0.8)
       ) bumpers

  -- The cup
  lineBody <- newEntity (StaticBody, Position (V2 5 (-3.9)))
  newEntity (Shape lineBody (hLine 1), Elasticity 0.9)
  lineBody <- newEntity (StaticBody, Position (V2 4.5 (-3.5)))
  newEntity (Shape lineBody (vLine 1), Elasticity 0.9)
  lineBody <- newEntity (StaticBody, Position (V2 5.5 (-3.5)))
  newEntity (Shape lineBody (vLine 1), Elasticity 0.9)

  -- The ball
  ball <- newEntity (DynamicBody, Position (V2 (-5) 5))
  newEntity (Shape ball (cCircle 0.2), Density 1, Elasticity 0.9)
  return ball


-- Run a graphical simulation using Gloss.
sim
  :: ( Has w IO Camera
     , Has w IO Physics
     )
  => Display
  -> System w ()
sim disp = do
  w <- ask
  liftIO $
     do
       t <- newIORef 0
       simulateIO disp
                white
                60
                w
                (\_     -> runSystem draw w)
                (\_ _ _ -> do
                    a <- readIORef t
                    -- Gloss takes a while to load so pause
                    -- for a few seconds before starting the simulation
                    if a <= 240 then modifyIORef t (+1) >> return w
                    else runSystem (stepPhysics (1/60)) w >> return w)
  where
    draw = do
      pic <- foldDrawM drawBody
      cam <- get global
      return $ cameraTransform cam pic

disp = InWindow "2d Physics example" (800,600) (10,10)

-- Given the bumper position, return the trajectory of the ball
-- (a list of (x,y) coordinates)
try :: [(Double,Double,Double)] -> IO [(Double,Double)]
try bumpers =
  do w <- initWorld
     runWith w $ do
       b <- initialize bumpers
       pos <- mapM (\_ -> do
                       stepPhysics (1/60)
                       (Position (V2 x y)) <- get b
                       return (x,y)
                   ) [1..1000]
       return $ takeWhile (\(_,y)->y>(-4.1)) pos


-- The model: Pick some bumper positions and angles uniformly
-- Run the 2d physics
-- Then observe that the ball lands more-or-less in the cup
model :: Meas [(Double,Double,Double)]
model = do
  bumpers <- mapM (\_ -> do
                      x <- sample
                      y <- sample
                      theta <- sample
                      return (x*10 - 5,y*10 -5,2*pi*theta)) [1..2]
  let (x,y) = unsafePerformIO $
              do { pos <- try bumpers ; return $ last pos }
  score $ normalPdf 5 0.2 x
  return bumpers

test3 = do
  samples <- mh model
  -- a non-MH simulation finds a solution after ~1000 steps
  traceShowM $ findIndex (\(_,w) -> w > 1) samples
  let Just (bumpers,weight) = find (\(_,w) -> w > 1) samples
  traceShowM bumpers
  traceShowM weight
  -- a non-MH simulation finds a solution after >10000 steps
  -- uncomment to try it
  -- samples <- weightedsamples model
  -- traceShowM $ findIndex (\(_,w) -> w > 1) samples
  w <- initWorld
  runSystem (initialize bumpers >> sim disp) w

test4 = do
  w <- initWorld
  runSystem (initialize [(4.4081374546901255,-4.800746160067368,5.0625809697752295),(-4.188382740275686,-2.115017318360092,2.739920917616955)] >> sim disp) w
