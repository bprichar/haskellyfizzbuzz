module FizzbuzzSpec (main, spec) where

import           Data.List  (isInfixOf)
import           Fizzbuzz   (fizzbuzz)
import           Test.Hspec
import           Text.Read

main = hspec spec

fizzbuzzes = take 100 fizzbuzz

spec :: Spec
spec =
    describe "fizzbuzz" $ do
        it "Each item is Fizz, Buzz, FizzBuzz or a number" $
            fizzbuzzes `shouldSatisfy` all possibleWordOrNumber

        it "If item is a number, it is the position in the list" $
            zip fizzbuzzes [1..] `shouldSatisfy` all ifNumberIsPosition

        it "Every third item has Fizz" $
            every 3 fizzbuzzes `shouldSatisfy` all ("Fizz" `isInfixOf`)

        it "Every fifth item has Buzz" $
            every 5 fizzbuzzes `shouldSatisfy` all ("Buzz" `isInfixOf`)

        it "Every fifteenth item has FizzBuzz" $
            every 15 fizzbuzzes `shouldSatisfy` all ("FizzBuzz" `isInfixOf`)

        it "If item is a number, its position is not divisible by 3" $
            zip fizzbuzzes [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 3)

        it "If item is a number, its position is not divisible by 5" $
            zip fizzbuzzes [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 5)

        it "If item is Fizz, its position is divisible by 3" $
            zip fizzbuzzes [1..] `shouldSatisfy` all ifIsFizzPositionDivisiblyBy3

        it "If item is Buzz, its position is divisible by 5" $
            zip fizzbuzzes [1..] `shouldSatisfy` all ifIsBuzzPositionDivisiblyBy5

        it "If item is FizzBuzz, its position is divisible by 15" $
            zip fizzbuzzes [1..] `shouldSatisfy` all ifIsFizzBuzzPositionDivisiblyBy15

isNumber :: String -> Bool
isNumber str = case readMaybe str :: Maybe Int of
    Just _  -> True
    Nothing -> False

possibleWordOrNumber :: String -> Bool
possibleWordOrNumber str = (str `elem` ["Fizz", "Buzz", "FizzBuzz"]) || isNumber str

ifNumberIsPosition :: (String, Int) -> Bool
ifNumberIsPosition (str, pos) = not (isNumber str) || (show pos == str)

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              []     -> []

ifIsNumberPositionNotDivisibleBy :: Int -> (String, Int) -> Bool
ifIsNumberPositionNotDivisibleBy divisor (str, pos) = not (isNumber str) || (pos `mod` divisor /= 0)

ifIsFizzPositionDivisiblyBy3 :: (String, Int) -> Bool
ifIsFizzPositionDivisiblyBy3 (str, pos) = str /= "Fizz" || (pos `mod` 3 == 0)

ifIsBuzzPositionDivisiblyBy5 :: (String, Int) -> Bool
ifIsBuzzPositionDivisiblyBy5 (str, pos) = str /= "Buzz" || (pos `mod` 5 == 0)

ifIsFizzBuzzPositionDivisiblyBy15 :: (String, Int) -> Bool
ifIsFizzBuzzPositionDivisiblyBy15 (str, pos) = str /= "FizzBuzz" || (pos `mod` 15 == 0)
