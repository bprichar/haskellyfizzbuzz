module CustomfizzbuzzSpec (main, spec) where

import           Customfizzbuzz  (customfizzbuzz)
import           Data.List
import           Test.Hspec
import           Test.QuickCheck
import           Text.Read

main = hspec spec

spec :: Spec
spec =
    describe "fizzbuzz" $ do
        it "Each item is a number, or some combination of the words" $
            forAll specGenerator propPossibleWordsOrNumbers

propPossibleWordsOrNumbers fizzbuzzspec = all (possibleWordOrNumber (possibleWords fizzbuzzspec)) (specToFizzbuzzes fizzbuzzspec)

possibleWordOrNumber :: [String] -> String -> Bool
possibleWordOrNumber possible str = (str `elem` possible) || isNumber str

isNumber :: String -> Bool
isNumber str = case readMaybe str :: Maybe Int of
    Just _  -> True
    Nothing -> False

possibleWords :: [(String, Int)] -> [String]
possibleWords fizzbuzzspec = map concat allCombinations
    where len' = length fizzbuzzspec :: Int
          words' = map fst fizzbuzzspec :: [String]
          allCombinations = concatMap (`combinations` words') [1..len']

combinations :: Int -> [a] -> [[a]]
combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

specToFizzbuzzes :: [(String, Int)] -> [String]
specToFizzbuzzes fizzbuzzspec = take (product (map snd fizzbuzzspec) * 3) (customfizzbuzz fizzbuzzspec)

specGenerator :: Gen [(String, Int)]
specGenerator = do
    n <- choose (1, 5)
    vectorOf n specPartGenerator

availableWords :: [String]
availableWords = ["Fizz", "Buzz", "Hiss", "Howl", "Foo", "Bar", "Baz", "Quux"]

somePrimes :: [Int]
somePrimes = [2, 3, 5, 7, 11, 13, 17, 23]

specPartGenerator :: Gen (String, Int)
specPartGenerator = do
    i <- choose (0, length availableWords - 1)
    j <- choose (0, length somePrimes - 1)
    let thisWord = availableWords !! i
    let thisPrime = somePrimes !! j
    return (thisWord, thisPrime)
