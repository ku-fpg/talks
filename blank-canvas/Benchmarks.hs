{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs #-}
module Benchmarks where

import Data.Default
import Control.Monad.IO.Class (liftIO)

import Diagrams.Prelude as Dia
import Diagrams.Backend.Canvas
import Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, renderableToFile,DEnv(..),runBackend)
import Graphics.Storyboard.Tile(Tile)
import Graphics.Rendering.Chart.Backend.Types (bitmapAlignmentFns)
import qualified Graphics.Storyboard as SB
import Graphics.Storyboard (Slide)

import Graphics.Storyboard.Diagrams()

slides :: [Slide ()]
slides =
 [ do SB.align SB.center $ SB.p $ "Diagrams Plugin Example"

      denv <- liftIO $ defaultEnv bitmapAlignmentFns 800 600

      let t = renderableToTile denv chart

      SB.place SB.top (SB.nudge SB.top SB.center t)

      return ()
 ]

dig :: Diagram B R2
dig = tournament 6

node :: Int -> Diagram B R2
node n = text (show n) # fontSizeN 0.1 # fc white
      <> circle 0.2 # fc green # named n

arrowOpts = with & gaps  .~ small
                 & headLength .~ Global 0.2

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

-- Chart.Renderable ()
chart :: Chart.Renderable ()
chart = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def


-- | Generate an image file for the given renderable, at the specified path. Size, format,
-- and text rendering mode are all set through the `FileOptions` parameter.
--renderableToTile:: (a ~ B, Dia.Renderable (Dia.Path R2) a, Backend a R2) => DEnv -> Chart.Renderable B -> Dia.Diagram a R2
renderableToTile:: DEnv -> Chart.Renderable () -> Tile ()
renderableToTile env r = SB.drawTile sz dig
  where
    sz = envOutputSize env
    cb = Chart.render r sz
    dig :: Dia.Diagram B R2
    (dig, _) = runBackend env cb
