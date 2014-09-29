{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Benchmarks where

import Data.Default
import Control.Monad.IO.Class (liftIO)
import Data.Csv

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Diagrams.Prelude as Dia
import Diagrams.Backend.Canvas
import Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Easy()  -- Hmm, this works

import Graphics.Rendering.Chart.Backend.Diagrams (runBackendWithGlyphs, defaultEnv, renderableToFile,DEnv(..),runBackend)
import Graphics.Storyboard.Tile(Tile)
import Graphics.Rendering.Chart.Backend.Types (bitmapAlignmentFns)
import qualified Graphics.Storyboard as SB
import Graphics.Storyboard (Slide)

import Graphics.Storyboard.Diagrams()

importResults :: IO [(String,String,Double)]
importResults = fmap concat $ do
   sequence [ do Right (_,vs :: V.Vector DataPoint) <- fmap decodeByName
                      $ BS.readFile
                      $ "blank-canvas/results/" ++ nm ++ ".csv"
                 return [ (nm,bm,val) | DataPoint bm val <- V.toList vs ]
            | os <- ["Linux","OSX"]
            , br <- ["Firefox","Chrome"]
            , bm <- ["JavaScript","BlankCanvas"]
            , let nm = os ++ "-" ++ br ++ "-" ++ bm
            ]

slides :: [Slide ()]
slides =
 [ do SB.align SB.center $ SB.p $ "Relative Performance of Blank Canvas"

      t <- cmpBenchmarks ".cache/display.png" [ "Bezier", "CirclesRandomSize", "CirclesUniformSize","FillText", "StaticAsteroids", "Image" ]

      SB.place SB.top (SB.nudge SB.top SB.center t)

      SB.align SB.center $ SB.p $ "Display-based operations"

      return ()

 , do SB.align SB.center $ SB.p $ "Relative Performance of Blank Canvas"

      t <- cmpBenchmarks ".cache/query.png" [ "IsPointInPath", "MeasureText", "Rave" ]

      SB.place SB.top (SB.nudge SB.top SB.center t)

      SB.align SB.center $ SB.p $ "Query-based operations"

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
chart :: [[Double]] -> [String] -> [String] -> Chart.Renderable ()
chart theData barTitles alabels = toRenderable layout
 where
  borders = False
  layout = id
      -- $ layout_title .~ "Sample Bars" ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double


  bars2 = id
      $ plot_bars_titles .~ (map (++ "\x2003\x2003\x2003") barTitles)
      $ plot_bars_values .~ addIndexes theData
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)


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
--    (dig, _, _) = runBackendWithGlyphs env cb


data DataPoint = DataPoint { name :: String, mean :: !Double }
  deriving (Show)

instance FromNamedRecord DataPoint where
     parseNamedRecord m = DataPoint <$>
                          m .: "Name" <*>
                          m .: "Mean"


cmpBenchmarks :: FilePath -> [String] -> Slide (Tile ())
cmpBenchmarks cacheName tests = do
    res <- liftIO $ importResults

    liftIO $ print res

    denv <- liftIO $ defaultEnv bitmapAlignmentFns (800/1) (500/1)

    let subTests = [ os ++ "-" ++ br :: String | os <- ["Linux","OSX"], br <- ["Firefox","Chrome"]]

    let theData =
          [ 1 : [ hs / js
                | st <- subTests
                , let [js] = [ v | (t1,t2,v) <- res, t1 == st ++ "-JavaScript", t2 == t ]
                , let [hs] = [ v | (t1,t2,v) <- res, t1 == st ++ "-BlankCanvas", t2 == t ]
                ]
          | t <- tests
          ]

    liftIO $ print theData
    {-
          [1,1.69,2.36,3.58,15.47]
          ,[1,1.31,2.84,1.49,5.22]
          ,[1,1.42,4.09,2.27,5.38]
          ,[1,1.20,2.77,1.50,3.93]
          ,[1,3.00,20.75,6.49,24.93]
          ,[1,9.81,7.34,2.84,8.67]
          ]
    -}
    let barTitles = ["Control"] ++ subTests

--    SB.cacheTile cacheName
    return $ SB.box SB.defaultBoxStyle $ renderableToTile denv $ chart theData barTitles tests
