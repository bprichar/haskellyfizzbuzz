module Customfizzbuzz (customfizzbuzz) where

customfizzbuzz :: [(String, Int)] -> [String]
customfizzbuzz specs = zipWith choice numbers (words' specs)

choice :: String -> String -> String
choice num word = if word == "" then num else word

numbers :: [String]
numbers = map show [1..]

words' :: [(String, Int)] -> [String]
words' = foldr (zipWith (++) . specToWordList) (repeat "")

specToWordList :: (String, Int) -> [String]
specToWordList (str, num) = cycle (replicate (num - 1) "" ++ [str])
