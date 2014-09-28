{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module BlankCanvas where

import Data.Monoid
import Graphics.Storyboard
import Graphics.Storyboard.Diagrams()

import qualified Diagrams as Diagrams
import Utils

slides :: [Slide ()]
slides =
 [  title "Blank Canvas" $ fontSize 24 $ do

    let s1 = do
          ul $ do
            li $ "Library for simple graphics"
            li $ "Uses HTML5's" <+> q "<canvas>" <+> "capabilities as viewport"
            li $ "Javascript examples can be transliterated into Haskell"
            li $ "Fast enough for basic animations"
          t1 <- scaledImageTile "blank-canvas/images/squares.png" 0.4
          place top $ nudge top center $ t1
    let s2 = fontSize 24 $ codeBox hs $ map ("    " ++)
                ["..."
                ,"translate (x,y)"
                ,"beginPath()"
                ,"moveTo(-100,-100)"
                ,"lineTo(-100,100)"
                ,"lineTo(100,100)"
                ,"lineTo(100,-100)"
                ,"closePath()"
                ,"lineWidth 10"
                ,"strokeStyle color"
                ,"stroke()"
                ,"..."
                ]

    twocol s1 0.55 s2 0.45

    ul $ li $ "Why another image processing tool?"
    align center $ p $ color "red" $ "Need a machine independent way of scripting up graphics in Haskell."

 , title "First Blank Canvas Example: red line" $ fontSize 24 $ do

    let s1 = do
            t1 <- scaledImageTile "blank-canvas/images/red-line.png" 0.8
            place top $ nudge top center $ t1
    let s2 = do
            fontSize 24 $ codeBox hs $
                ["import Graphics.Blank"
                ,""
                ,"main = blankCanvas 3000 $ \\ context -> do"
                ,"        send context $ do"
                ,"                moveTo(50,50)"
                ,"                lineTo(200,100)"
                ,"                lineWidth 10"
                ,"                strokeStyle \"red\""
                ,"                stroke()"
                ]

    twocol s1 0.40 s2 0.60

    pause
    p $ "First, include the" <+> q "Graphics.Blank" <+> "module."
    pause
    p $ "Second, main calls" <+> q "blankCanvas" <> ", with two arguments"
    ul $ do
      li $ "The port (3000) to publish the application on; and"
      li $ "and what to do with canvas."
    pause
    p $ "Third, we" <+> q "send" <+> "to the canvas a list of monadic commands."
    pause
    p $ "Finally, there are the commands to draw a" <+> color "red" "red" <+> "line."

 , title "Architecture of blank-canvas" $ fontSize 24 $ do

    -- block of the bottom
    (w,h) <- getCavitySize
    place bottom $ blank (0,h - 450)

    lg <- sky_blue

    let hdr = background lg . td .align center . b . p
    let dat = td . align left . leftMargin 5 . rightMargin 5 . p
    let dat' = td . align center . leftMargin 5 . rightMargin 5 . p

    t0 <- tileOfSlide (400,0) $ table
        [ tr [ dat' $ "Your application:" </> quote "please draw red line"]]

    t1 <- tileOfSlide (400,0) $ table
        [ tr [ hdr $ "blank-canvas"]
        , tr [ dat $ "Canvas commands for drawing pictures"]
        , tr [ hdr $ "kansas-comet"]
        , tr [ dat $ "Comet pattern for pushing to client"]
        , tr [ hdr $ "scotty"]
        , tr [ dat $ "Support for RESTful APIs."]
        , tr [ hdr $ "warp"]
        , tr [ dat $ "Receiving web requests from clients; give back responses & web pages."]
        ]

    place left $ blank (50,0)
    place left $ column $ [t0,t1,blank (0,0)]

    t2 <- scaledImageTile "blank-canvas/images/squares.png" 0.4

    place right $ blank (50,0)
    place right $ (nudge bottom right t2)


    t2 <- tileOfSlide (300,200) $ align center $ do
          p $ "HTTP link (typically local)"
          fontSize 80 $ p $ "\x27FA"

    place bottom $ t2

    ]
