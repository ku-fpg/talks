module Main (main) where

main = print (fib n)
  where
    n = 42

fib :: Int -> Int
fib n = if n < 2 then 1 else fib(n-1) + fib (n-2)
