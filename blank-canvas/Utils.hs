{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Applicative
import Graphics.Blank hiding (font)
import Control.Concurrent.STM

import Graphics.Storyboard

cavitySizeSlide :: Slide ()
cavitySizeSlide = do
  sz <- getCavitySize
  margin 20 $ fontSize 50 $ p $ prose $ (show sz)

title :: Prose -> Slide () -> Slide ()
title txt slide = font "Gill Sans" $ do
  lg <- ku_blue
  background lg
    $ shadows False
    $ borderWidth 0
    $ frame
    $ scaleFont 2
    $ align center
    $ color "white"
    $ p
    $ txt
  fullSlide slide

fullSlide :: Slide () -> Slide ()
fullSlide slide = font "Gill Sans" $ do
  img <- imageTile "images/brand/KUlogo1C.png"
  place right $ pack $ anchor bottom $ point bottom right $ img
  ss <- askSlideStyle
  let pg = prose $ show $ theSlideNumber $ ss
  let lastPg = prose $ show $ theLastSlide $ ss
  t <- tileOfSlide (200,20) $ fontSize 10 $ p
     $ " \xa9" <> "2014 Andrew Gill," <+> pg <> "/" <> lastPg
  place bottom $ point bottom left $ t
  margin 20 $ fontSize 20 $ slide

ku_blue :: Slide Background
ku_blue = bgLinear "#0002D4" "#004294" -- "#0022B4"

sky_blue :: Slide Background
sky_blue = bgLinear "#7DC6D7" "#5DC6F7" -- "#6DC6E7"

sand :: Slide Background
sand = return (bgColor "#DAD6CB") --  bgLinear "#EAD6BB" "#CAD6DB" -- #DAD6CB

h :: Int -> Prose -> Slide ()
h n = scaleFont  (1 + 0.2 * (7 - fromIntegral n)) . p

quote :: Prose -> Prose
quote p = "\x201C" <> p <> "\x201D"

parens :: Prose -> Prose
parens p = "(" <> p <> ")"

rightdoublearrow :: Prose
rightdoublearrow = "\x21D2"

leftdoublearrow :: Prose
leftdoublearrow = "\x21D0"

rightarrow :: Prose
rightarrow = "\x2192"

leftarrow :: Prose
leftarrow = "\x2190"

pi :: Prose
pi = "\x3c0"

lambda :: Prose
lambda = "\x3bb"

forallchar :: Prose
forallchar = "\x2200"

nbsp :: Prose
nbsp = "\x00A0"

desc t o = do
      p $ b $ t
      indent $ p o
      vspace 28

data TitleSlideEnv = TitleSlideEnv
     { titleTitle    :: Prose
     , titleSubTitle :: Prose
     , titleAuthors  :: Prose
     , titleDate     :: Prose
     }

titleSlide :: TitleSlideEnv -> Slide ()
titleSlide t = fullSlide $ align center $ do

    fontSize 72 $ p $ titleTitle $ t
    vspace 28
    fontSize 48 $ p $ titleSubTitle $ t
    vspace 100
    fontSize 28 $ p $ titleAuthors $ t
    vspace 18
    fontSize 24 $ p $ "The University of Kansas"
    vspace 18
    fontSize 18 $ p $ titleDate $ t
    vspace 48
    img <- imageTile "images/brand/jhwk_LF_200px.gif"
    place top (nudge top center img)

q :: ProseStyle a => a -> a
q = font "monospace" . b

data CodeBox = CodeBox
        { cb_bg    :: Slide Background
        , cb_title :: Maybe Prose
        , cb_highlight :: TheHighlightStyle
        }

ghci :: CodeBox
ghci = CodeBox { cb_bg = sand, cb_title = Nothing, cb_highlight = ghciHighlightStyle }

hs :: CodeBox
hs = CodeBox { cb_bg = sky_blue, cb_title = Nothing,  cb_highlight = haskellHighlightStyle }

verb :: CodeBox
verb = CodeBox { cb_bg = sky_blue, cb_title = Nothing,  cb_highlight = defaultHighlightStyle }


codeBox :: CodeBox -> [String] -> Slide ()
codeBox cb txt = do
  lg <- cb_bg cb
  vspace 10
  font "Courier New" $ trueSpace $ do
    background lg $ frame $ margin 10 $ do
      p $ highlight (cb_highlight cb)
            $ unlines
            $ txt
  vspace 10

codeBox' :: CodeBox -> Prose -> [String] -> Slide ()
codeBox' cb t txt = do
  lg <- cb_bg cb
  vspace 10
  font "Courier New" $ trueSpace $ table
      [ tr [ background lg $ td $ b $ margin 5 $ p $ t]
      , tr [ td $ margin 5
                $ p
                $ highlight (cb_highlight cb)
                $ unlines
                $ txt
        ]
      ]
  vspace 10


twocol :: Slide () -> Double -> Slide () -> Double -> Slide ()
twocol col1 w1 col2 w2 = do
  (w,_) <- getCavitySize

  t1 <- tileOfSlide (w * w1,0) col1
  t2 <- tileOfSlide (w * w2,0) col2

  place top (pack (anchor left t1 <> anchor right t2))


twobox :: Prose -> Prose -> Prose -> Slide ()
twobox lhs txt rhs = do
  vspace 10
  fontSize 40 $ leftMargin 100 $ rightMargin 100 $ table $
      [tr [background (bgColor "transparent") $ borderWidth 0 $ shadows False $ td $ align center $ p $ lhs
          ,background (bgColor "yellow") $ td $ do align center $ p $ txt
          ,background (bgColor "transparent") $ borderWidth 0 $ shadows False $ td $ align center $ p $ rhs
          ]
      ]
  vspace 10
