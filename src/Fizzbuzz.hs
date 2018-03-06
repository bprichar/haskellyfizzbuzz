module Fizzbuzz (fizzbuzz) where

fizzes = cycle ["", "", "Fizz"]

buzzes = cycle ["", "", "", "", "Buzz"]

words' = zipWith (++) fizzes buzzes

numbers = map show [1..]

choice word num = if word == "" then num else word

fizzbuzz = zipWith choice words' numbers
