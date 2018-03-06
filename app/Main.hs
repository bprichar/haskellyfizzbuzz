module Main where

import           Fizzbuzz (fizzbuzz)

main :: IO ()
main = mapM_ putStrLn $ take 100 fizzbuzz
