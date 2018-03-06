module FizzbuzzhisshowlSpec (main, spec) where

import           Data.List        (isInfixOf)
import           Fizzbuzzhisshowl (fizzbuzzhisshowl)
import           Test.Hspec
import           Text.Read

main = hspec spec

thelist = take 10000 fizzbuzzhisshowl

spec :: Spec
spec =
    describe "fizzbuzzhishowl" $ do
        it "Each item is one of the possible cominations of Fizz, Buzz, Hiss and Howl, or a number" $
            thelist `shouldSatisfy` all possibleWordOrNumber

        it "If item is a number, it is the position in the list" $
            zip thelist [1..] `shouldSatisfy` all ifNumberIsPosition

        it "Every third item has Fizz" $
            every 3 thelist `shouldSatisfy` all ("Fizz" `isInfixOf`)

        it "Every fifth item has Buzz" $
            every 5 thelist `shouldSatisfy` all ("Buzz" `isInfixOf`)

        it "Every seventh item has Hiss" $
            every 7 thelist `shouldSatisfy` all ("Hiss" `isInfixOf`)

        it "Every eleventh item has Howl" $
            every 11 thelist `shouldSatisfy` all ("Howl" `isInfixOf`)

        it "Every fifteenth item has FizzBuzz" $
            every 15 thelist `shouldSatisfy` all ("FizzBuzz" `isInfixOf`)

        it "Every 35th item has BuzzHiss" $
            every 35 thelist `shouldSatisfy` all ("BuzzHiss" `isInfixOf`)

        it "Every 77th item has HissHowl" $
            every 77 thelist `shouldSatisfy` all ("HissHowl" `isInfixOf`)

        it "Every 105th item has FizzBuzzHiss" $
            every 105 thelist `shouldSatisfy` all ("FizzBuzzHiss" `isInfixOf`)

        it "Every 385th item has BuzzHissHowl" $
            every 385 thelist `shouldSatisfy` all ("BuzzHissHowl" `isInfixOf`)

        it "Every 1155th item has FizzBuzzHissHowl" $
            every 1155 thelist `shouldSatisfy` all ("FizzBuzzHissHowl" `isInfixOf`)

        it "If item is a number, its position is not divisible by 3" $
            zip thelist [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 3)

        it "If item is a number, its position is not divisible by 5" $
            zip thelist [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 5)

        it "If item is a number, its position is not divisible by 7" $
            zip thelist [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 7)

        it "If item is a number, its position is not divisible by 11" $
            zip thelist [1..] `shouldSatisfy` all (ifIsNumberPositionNotDivisibleBy 11)

        it "If item is Fizz, its position is divisible by 3" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "Fizz" 3)

        it "If item is Buzz, its position is divisible by 5" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "Buzz" 5)

        it "If item is Hiss, its position is divisible by 7" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "Hiss" 7)

        it "If item is Howl, its position is divisible by 11" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "Howl" 11)

        it "If item is FizzBuzz, its position is divisible by 15" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzBuzz" 15)

        it "If item is FizzHiss, its position is divisible by 21" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzHiss" 21)

        it "If item is FizzHowl, its position is divisible by 33" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzHowl" 33)

        it "If item is BuzzHiss, its position is divisible by 35" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "BuzzHiss" 35)

        it "If item is BuzzHowl, its position is divisible by 55" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "BuzzHowl" 55)

        it "If item is HissHowl, its position is divisible by 77" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "HissHowl" 77)

        it "If item is FizzBuzzHiss, its position is divisible by 105" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzBuzzHiss" 105)

        it "If item is FizzBuzzHowl, its position is divisible by 165" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzBuzzHowl" 165)

        it "If item is FizzHissHowl, its position is divisible by 231" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzHissHowl" 231)

        it "If item is BuzzHissHowl, its position is divisible by 385" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "BuzzHissHowl" 385)

        it "If item is FizzBuzzHissHowl, its position is divisible by 1155" $
            zip thelist [1..] `shouldSatisfy` all (ifIsStringPositionDivisibleBy "FizzBuzzHissHowl" 1155)

isNumber :: String -> Bool
isNumber str = case readMaybe str :: Maybe Int of
    Just _  -> True
    Nothing -> False

possibleWords = ["Fizz",
                 "Buzz",
                 "Hiss",
                 "Howl",
                 "FizzBuzz",
                 "FizzHiss",
                 "FizzHowl",
                 "BuzzHiss",
                 "BuzzHowl",
                 "HissHowl",
                 "FizzBuzzHiss",
                 "FizzBuzzHowl",
                 "FizzHissHowl",
                 "BuzzHissHowl",
                 "FizzBuzzHissHowl"]

possibleWordOrNumber :: String -> Bool
possibleWordOrNumber str = (str `elem` possibleWords) || isNumber str

ifNumberIsPosition :: (String, Int) -> Bool
ifNumberIsPosition (str, pos) = not (isNumber str) || (show pos == str)

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              []     -> []

ifIsNumberPositionNotDivisibleBy :: Int -> (String, Int) -> Bool
ifIsNumberPositionNotDivisibleBy divisor (str, pos) = not (isNumber str) || (pos `mod` divisor /= 0)

ifIsStringPositionDivisibleBy :: String -> Int -> (String, Int) -> Bool
ifIsStringPositionDivisibleBy comp divisor (str, pos) = str /= comp || (pos `mod` divisor == 0)
