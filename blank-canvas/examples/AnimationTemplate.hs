module Main where

import Graphics.Blank

main = blankCanvas 3000 $ \ context -> loop context (0 :: Float)

loop context n = do
        send context $ do
                -- clear the canvas
                ...
                -- draw the square
                ...
                
        loop context (n + 0.01)
