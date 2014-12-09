{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import System.Environment

import Graphics.Storyboard

import Utils
import qualified Hermit
--import qualified BlankCanvas

main :: IO ()
main = do
  args <- getArgs
  main2 args

main2 :: [String] -> IO ()
main2 [] = main2 ["wg21","0"]
main2 [set] = main2 [set,"0"]
main2 [set,start] =
    case lookup set slides of
    Just sts -> storyBoard 3000 { snapShot = Nothing } $ drop (read start) $ sts
    Nothing -> error "can not finds slides"

slides :: [(String,[Slide ()])]
slides =
    [("wg21",[Utils.titleSlide $ TitleSlideEnv
   			{ titleTitle    = fontSize 50 $ "Worker/Wrapper for a Better Life"
			, titleSubTitle = fontSize 36 $ "Using HERMIT to transform Haskell programs in a post-hoc manner"
			, titleAuthors  = b "Andrew Gill" <> ",  and Andrew Farmer, Mike Stees and Brad Torrence"
			, titleDate     = "December 9" <> super "th" <+> "2014"
			}
        ]
          ++ Hermit.slides
--          ++ Benchmarks.slides
          ++
        [ ]
     )
    ]
