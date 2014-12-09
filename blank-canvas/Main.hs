{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import System.Environment

import Graphics.Storyboard

import Utils
import qualified Benchmarks
import qualified BlankCanvas

{-


import qualified Intro
import qualified IntroGHCi
import qualified IntroTypes
import qualified IntroStructure
import qualified IntroFunctions
import qualified Homeworks
import qualified HigherOrder
import qualified UserTypes
-}

main :: IO ()
main = do
  args <- getArgs
  main2 args

main2 :: [String] -> IO ()
main2 [] = main2 ["ifl14","0"]
main2 [set] = main2 [set,"0"]
main2 [set,start] =
    case lookup set slides of
    Just sts -> storyBoard 3001 { snapShot = Nothing } $ drop (read start) $ sts
    Nothing -> error "can not finds slides"

slides :: [(String,[Slide ()])]
slides =
    [("ifl14",[Utils.titleSlide $ TitleSlideEnv
   			{ titleTitle    = fontSize 50 $ "Blank Canvas and the\nRemote-Monad Design Pattern"
				, titleSubTitle = fontSize 36 $ "A Foreign Function Interface to the JavaScript Canvas API"
				, titleAuthors  = b "Andrew Gill" <> ", Aleksander Eskilson, Ryan Scott, James Stanton"
				, titleDate     = "October 1" <> super "st" <+> "2014"
				}
        ]
          ++ BlankCanvas.slides
          ++ Benchmarks.slides
          ++
        [ title "Conclusions" $ fontSize 32 $ do

           p $ "We have constructed a fully featured API bridge to the HTML canvas"

           vspace 10

           p $ "The API is fast enough for teaching, and casual games, and slide presentations"

           vspace 10

           p $ "Round-trip commands should be avoided where possible"

           vspace 10

           p $ "This API design depends on monad reification"

           vspace 10

           p $ "There are some interesting semantic issues to be elaborated for" <+> q "send"

           vspace 15

           align center $ fontSize 40 $ p $ q "cabal install blank-canvas"

        ]
     )
    ]
