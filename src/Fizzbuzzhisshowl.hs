module Fizzbuzzhisshowl (fizzbuzzhisshowl) where

fizzes = cycle ["", "", "Fizz"]

buzzes = cycle ["", "", "", "", "Buzz"]

hisses = cycle ["", "", "", "", "", "", "Hiss"]

howls = cycle ["", "", "", "", "", "", "", "", "", "", "Howl"]

words' = zipWith (++) (zipWith (++) (zipWith (++) fizzes buzzes) hisses) howls

numbers = map show [1..]

choice word num = if word == "" then num else word

fizzbuzzhisshowl = zipWith choice words' numbers
