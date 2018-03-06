module Customfizzbuzz
(
    FizzbuzzSpec,
    FizzbuzzSpecPart,
    customfizzbuzz,
)
where

type FizzbuzzSpecPart = (String, Int)

type FizzbuzzSpec = [FizzbuzzSpecPart]

customfizzbuzz :: FizzbuzzSpec -> [String]
customfizzbuzz specs = zipWith choice numbers (words' specs)

choice :: String -> String -> String
choice num word = if word == "" then num else word

numbers :: [String]
numbers = map show [1..]

words' :: FizzbuzzSpec -> [String]
words' = foldr (zipWith (++) . specToWordList) (repeat "")

specToWordList :: FizzbuzzSpecPart -> [String]
specToWordList (str, num) = cycle (replicate (num - 1) "" ++ [str])
