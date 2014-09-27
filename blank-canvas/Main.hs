{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import System.Environment

import Graphics.Storyboard

import Utils
import qualified Benchmarks

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
    Just sts -> storyBoard 3000 { snapShot = Nothing } $ drop (read start) $ sts
    Nothing -> error "can not finds slides"

slides :: [(String,[Slide ()])]
slides =
    [("ifl14",[Utils.titleSlide $ TitleSlideEnv
   			{ titleTitle    = fontSize 50 $ "Blank Canvas and the remote-monad design pattern"
				, titleSubTitle = fontSize 36 $ "A Foreign Function Interface to the JavaScript Canvas API"
				, titleAuthors  = b "Andrew Gill" <> ", Aleksander Eskilson, Ryan Scott, James Stanton"
				, titleDate     = "August 25" <> super "th" <+> "2014"
				}
        ] ++ Benchmarks.slides
     )
    ]