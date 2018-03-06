module Fizzbuzzhisshowl (fizzbuzzhisshowl) where

specs = [("Fizz", 3), ("Buzz", 5), ("Hiss", 7), ("Howl", 11)]

words' = foldr (zipWith (++) . specToWordList) (repeat "") specs

numbers = map show [1..]

choice word num = if word == "" then num else word

fizzbuzzhisshowl = zipWith choice words' numbers

specToWordList :: (String, Int) -> [String]
specToWordList (str, num) = cycle (replicate (num - 1) "" ++ [str])
