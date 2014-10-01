{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module BlankCanvas where

import Control.Monad.IO.Class (liftIO)

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

    fsz <- 24 `vga` 22
    let s2 = do
            fontSize fsz $ codeBox hs $
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

 , title "Architecture of Blank Canvas" $ fontSize 24 $ do

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

    cavityWidth 1000

    place left $ column $ [t0,t1,blank (0,0)]

    t2 <- scaledImageTile "blank-canvas/images/squares.png" 0.4

    place right $ (nudge bottom right t2)


    t2 <- tileOfSlide (300,200) $ align center $ do
          p $ "HTTP link\n(typically local)"
          fontSize 80 $ p $ "\x27FA"

    place bottom $ t2

 , title "Types in Blank Canvas" $ fontSize 24 $ do

    txt <- liftIO $ readFile "blank-canvas/CodeFragments.hs"
    fontSize 28 $ codeBox hs $ take 13 $ drop 0 $ lines $ txt

 , title "Flow of Types" $ fontSize 32 $ do

    fontSize 40 $ leftMargin 100 $ rightMargin 100 $ frame $ align center $ p $ q "send ::  Context -> Canvas a -> IO a"

    vspace 25

    ul $ do
      li $ "Drawings are constructed using the" <+> q "Canvas" <+> "monad"
      li $ q"send" <+>"pushes a" <+> q"Canvas" <+> "drawing to the screen"
      li $ "No way to get from inside the" <+> q "Canvas" <+> "monad to the" <+> q "IO" <+> "monad"
      li $ "Hence, programs are written in" <+> q "IO" <> ", with islands of" <+> q"Canvas" <+> "drawing rendered via send  "
      li $ q "Canvas" <+> "results are returned to" <+> q "IO" <+> "via send."

 , title "Example of Javascript to Haskell Transliteration" $ fontSize 24 $ do

--    let hdr = background lg . td .align center . b . p
--    let dat = td . align left . leftMargin 5 . rightMargin 5 . p

    t1 <- tileOfSlide (400,0) $ codeBox' verb "JavaScript"
      ["context.moveTo(100, 150);"
      ,"context.lineTo(450, 50);"
      ,"context.stroke();"
      ]

    t2 <- tileOfSlide (400,0) $ codeBox' verb "Haskell"
      ["send context $ do"
      ,"  moveTo(100, 150)"
      ,"  lineTo(450, 50)"
      ,"  stroke()"
      ]


    t3 <- tileOfSlide (600,0) $ codeBox' verb "Types in blank-canvas"
      ["moveTo :: (Double,Double) -> Canvas ()"
      ,"lineTo :: (Double,Double) -> Canvas ()"
      ,"stroke :: ()              -> Canvas ()"
      ,""
      ,"send   :: Context -> Canvas a -> IO a"
      ]

    place top $ row [blank(0,0),t1,t2,blank(0,0)]

    vspace 20

    place top $ row [blank(0,0),t3,blank(0,0)]

    --let p1 = fontSize 24 $ codeBox hs $
 , title "Supported Canvas Commands" $ fontSize 24 $ do

    t <- scaledImageTile "blank-canvas/images/HTML5_Canvas_Cheat_Sheet.png" 0.6

    place top $ nudge top center $ box defaultBoxStyle $ t


 , title "Animation" $ fontSize 32 $ do

    p $ "Animation is simply a matter of sending draw commands fast enough to the browser window."

    txt <- liftIO $ readFile "blank-canvas/examples/AnimationTemplate.hs"
    fontSize 24 $ codeBox hs $ lines $ txt

 , title "Rotating Square" $ fontSize 32 $ do
    t1 <- scaledImageTile "blank-canvas/images/1.png" 0.3
    t2 <- scaledImageTile "blank-canvas/images/3.png" 0.3
    t3 <- scaledImageTile "blank-canvas/images/5.png" 0.3

    cavityWidth 1000

    place left (nudge top left $ column [t1,t2,t3])

    place left (blank (100,0))

    txt <- liftIO $ readFile "blank-canvas/examples/RotatingTile.hs"
    fontSize 18 $ codeBox hs $ lines $ txt


 , title "Interaction" $ fontSize 32 $ do

    p $ "So far, everything has about been" <+> b "sending" <+> "JavaScript commands."

    p $ "There are two types of browser" <> rightarrow <> "application interaction."

    vspace 10

    b $ p $ "The HTML canvas, if asked, can listen for events."
    ul $ do
      li $ "Examples are" <+> q "mousedown" <+> "and" <+> q "mousemove" <> "."
      li $ "We need to send the events to the Haskell program."
      li $ "We handle events using a" <+> q "STM" <+> "Queue."

    vspace 10

    b $ p $ "We can also query the JavaScript state from inside" <+> q "Canvas"
    ul $ do
      li $ "For example, is a point inside a path?"

      vspace 10

      rightMargin 200 $ fontSize 18 $ codeBox hs ["isPointInPath :: (Double, Double) -> Canvas Bool"]

      vspace 10

      li $ "Question: How do we get the boolean back to Haskell?"

 , title (q "Canvas" <+> "is a Monad") $ fontSize 32 $ do

    p $ q "Canvas" <> ", as a monad, has a bind and return."


    fsz <- 30 `vga` 24
    fontSize fsz $ codeBox hs
        ["moveTo        :: (Double,Double) -> Canvas ()"
        ,"lineTo        :: (Double,Double) -> Canvas ()"
        ,"stroke        :: ()              -> Canvas ()"
        ,"isPointInPath :: (Double,Double) -> Canvas Bool"
        ,""
        ,"return        :: a -> Canvas a"
        ,"(>>=)         :: Canvas a -> (a -> Canvas b) -> Canvas b"
        ,""
        ,"send :: Context -> Canvas a -> IO a -- completely polymorphic"
        ]

    ul $ do
      li $ "How does" <+> q "isPointInPath" <+> "return a result?"
      li $ "Can" <+> q "send" <+> "truly be polymophic?"


 , title "Monadic Reification" $ fontSize 20 $ do


    t1 <- tileOfSlide (800,0) $
          fontSize 24 $ codeBox hs
            ["(>>=) :: Canvas a -> (a -> Canvas b) -> Canvas b"
            ,"send :: Context -> Canvas a -> IO a"
            ]

    place top $ nudge top center $ t1

    let s1 = do

          p $ "Don't we need to know the type of 'a' to reify the commands?"
          vspace 10
          p $ "How do we constrain the type of 'a' in" <+> q "(>>=)" <+> "and" <+> q "send" <> "?"

          vspace 10

          p $ b "Solution:" <+> "Normalize and reify the monad!"

          vspace 10

          p $ "See: The Constrained-Monad Problem, ICFP'13, Sculthorpe, et. al. for details."

        s2 = do
          t1 <- scaledImageTile "blank-canvas/images/MonadNormalForm.png" 0.8
          place top $ nudge top center $ box defaultBoxStyle $ t1

    twocol s1 0.55 s2 0.45


 , title "The Canvas Monad" $ fontSize 18 $ do

    txt <- liftIO $ readFile "blank-canvas/examples/InsideCanvas.hs"
    codeBox hs $ lines $ txt

 , title "GADTs to the rescue!" $ fontSize 14 $ do

    txt <- liftIO $ readFile "blank-canvas/examples/InsideSend.hs"
    codeBox hs $ lines $ txt

 , title ("Semantics of" <+> q "send") $ fontSize 28 $ do

    p $ "First Law: (1)"

    vspace 10
    fontSize 36 $ leftMargin 100 $ rightMargin 100 $ frame $ align center $ p
        $ q "send (return a) = return a"

    vspace 10

    ul $ do
      li $ "a" <+> q "send" <+> "has no effect except the remote commands"

    vspace 25

    p $ "Second Law: (2)"

    vspace 10
    fontSize 36 $ leftMargin 100 $ rightMargin 100 $ frame $ align center $ p
      $ q "send m1 >>= send m2 = send (m1 >>= m2)"
    vspace 10

    ul $ do
      li $ "Remote commands preserve ordering"
      li $ "Remove commands can be split and joined into different sized packets"
      li $ b "There is a pre-condition of non-interference"

    vspace 10

    p $ "Interesting relationship to Software Transactional Memories (" <> q "atomically" <> ")," <+>
        "the ST monad ("<> q "runST" <> ") and IO ("<> q "forkIO" <> ")."

    return ()

 , title "Design Principles" $ fontSize 32 $ do

    p $ "The packet principles are:"
    ul $ do
      li $ "Where possible, everything in a" <+> q"send" <> "-packet should be sent to be executed together."
      li $ "The breaks between packets should be deterministic and statically and/or syntactically determinable"
      li $ "Packets are not combined between different calls to" <+> q "send."

    vspace 10

    p $ "The command principles are:"
    ul $ do
      li "Anything that returns () is asynchronous, and may be combined with the next monadic command, or sent instantly."
      li "Anything that does not return () is synchronous, and requires a round-trip to the server. "


 , title "Conclusions" $ fontSize 32 $ do

    p $ "We have constructed a fully featured API bridge to the HTML canvas"

    vspace 10

    p $ "The API is fast enough for teaching, and casual games, and slide presentations"

    vspace 10

    p $ "Round-trip commands should be avoided where possible"

    vspace 10

    p $ "This API design is possible because of monad reification"

    vspace 10

    p $ "There are some interesting semantic issues to be elaborated for" <+> q "send"

    vspace 15

    align center $ fontSize 40 $ p $ q "cabal install blank-canvas"

 ]
