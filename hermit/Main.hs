{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import System.Environment

import Graphics.Storyboard

import Utils
--import qualified Benchmarks
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
			, titleAuthors  = b "Andrew Gill" <> ", Brad Torrence, Mike Stees and Andrew Farmer"
			, titleDate     = "December 9" <> super "th" <+> "2014"
			}
        ]
--          ++ BlankCanvas.slides
--          ++ Benchmarks.slides
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
