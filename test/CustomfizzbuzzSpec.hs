module CustomfizzbuzzSpec (main, spec) where

import           Customfizzbuzz             (FizzbuzzSpec, FizzbuzzSpecPart,
                                             customfizzbuzz)
import           Data.List
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Text.Read

main = hspec spec

spec :: Spec
spec =
    describe "fizzbuzz"$ modifyMaxSuccess (const 10) $ do
        it "Each item is a number, or some combination of the words" $
            forAll specGenerator propPossibleWordsOrNumbers

        it "If item is a number, it is the position in the list" $
            forAll specGenerator propIfNumberIsPosition

        it "Each word is included every specified position" $
            forAll specGenerator propEachWordAtEveryNthPosition

specGenerator :: Gen FizzbuzzSpec
specGenerator = do
    n <- choose (1, 4)
    vectorOf n specPartGenerator

specPartGenerator :: Gen FizzbuzzSpecPart
specPartGenerator = do
    i <- choose (0, length availableWords - 1)
    j <- choose (0, length somePrimes - 1)
    let thisWord = availableWords !! i
    let thisPrime = somePrimes !! j
    return (thisWord, thisPrime)

availableWords :: [String]
availableWords = ["Fizz", "Buzz", "Hiss", "Howl", "Foo", "Bar", "Baz", "Quux"]

somePrimes :: [Int]
somePrimes = [2, 3, 5, 7, 11, 13, 17]

propPossibleWordsOrNumbers :: FizzbuzzSpec -> Bool
propPossibleWordsOrNumbers fizzbuzzspec =
    all
        (possibleWordOrNumber (possibleWords fizzbuzzspec))
        (specToFizzbuzzes fizzbuzzspec)

possibleWordOrNumber :: [String] -> String -> Bool
possibleWordOrNumber possible str = (str `elem` possible) || isNumber str

isNumber :: String -> Bool
isNumber str = case readMaybe str :: Maybe Int of
    Just _  -> True
    Nothing -> False

possibleWords :: FizzbuzzSpec -> [String]
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

specToFizzbuzzes :: FizzbuzzSpec -> [String]
specToFizzbuzzes fizzbuzzspec =
    take
        (product (map snd fizzbuzzspec) * 3)
        (customfizzbuzz fizzbuzzspec)

propIfNumberIsPosition :: FizzbuzzSpec -> Bool
propIfNumberIsPosition fizzbuzzspec =
    all ifNumberIsPosition (zip (specToFizzbuzzes fizzbuzzspec) [1..])

ifNumberIsPosition :: (String, Int) -> Bool
ifNumberIsPosition (str, pos) = not (isNumber str) || (show pos == str)

propEachWordAtEveryNthPosition :: FizzbuzzSpec -> Bool
propEachWordAtEveryNthPosition fizzbuzzspec =
    all
        (wordAtEveryNthPosition (specToFizzbuzzes fizzbuzzspec))
        fizzbuzzspec

wordAtEveryNthPosition :: [String] -> FizzbuzzSpecPart -> Bool
wordAtEveryNthPosition fizzbuzzes (wrd, freq) =
    all (wrd `isInfixOf`) (every freq fizzbuzzes)

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
    (y:ys) -> y : every n ys
    []     -> []
