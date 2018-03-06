module Main where

import           Customfizzbuzz (customfizzbuzz)

main :: IO ()
main = mapM_ putStrLn $ take 100 (customfizzbuzz [("Fizz", 3), ("Buzz", 5)])
