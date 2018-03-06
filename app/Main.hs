module Main where

import           Fizzbuzzhisshowl (fizzbuzzhisshowl)

main :: IO ()
main = mapM_ putStrLn $ take 100 fizzbuzzhisshowl
