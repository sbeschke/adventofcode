module Main where

import Data.List.Split
import qualified Data.Map as Map


main :: IO ()
main = do
    content <- readFile "input"
    let [rulesBlob, statementsBlob] = splitOn "\n\n" content
    let initialRules = map parseRule (lines rulesBlob)
    let rules = combineRules initialRules

    let statements = map parseStatement (lines statementsBlob)
    let valid = filter (checkStatement rules) statements
    print $ sum (map extractMiddle valid)

    let invalid = filter (not . checkStatement rules) statements
    let fixed = map (fixStatement rules) invalid
    print $ sum (map extractMiddle fixed)

combineRules :: [(Integer, Integer)] -> Map.Map Integer [Integer]
combineRules = addRules Map.empty

addRules :: Map.Map Integer [Integer] -> [(Integer, Integer)] -> Map.Map Integer [Integer]
addRules = foldr (\(a, b) m -> Map.insertWith (++) a [b] m)

parseRule :: String -> (Integer, Integer)
parseRule s =
    case splitOn "|" s of
        [l, r] -> (read l :: Integer, read r :: Integer)

parseStatement :: String -> [Integer]
parseStatement s = map (\x -> read x :: Integer) $ splitOn "," s

checkStatement :: Map.Map Integer [Integer] -> [Integer] -> Bool
checkStatement m (i:is) =
    let pairs = [(i, b) | b <- is]
    in all (checkPair m) pairs && checkStatement m is
checkStatement _ _ = True

checkPair :: Map.Map Integer [Integer] -> (Integer, Integer) -> Bool
checkPair m (a, b) = a `notElem` Map.findWithDefault [] b m

extractMiddle :: [a] -> a
extractMiddle l = l !! (fromIntegral (length l) `div` 2)

violatingPairs :: Map.Map Integer [Integer] -> [Integer] -> [(Integer, Integer)]
violatingPairs m (i:is) = [(i, b) | b <- is, not (checkPair m (i, b))] ++ violatingPairs m is
violatingPairs _ _ = []

swapPair :: (Integer, Integer) -> [Integer] -> [Integer]
swapPair (a, b) (x:xs) | x == a = b:swapPair (a, b) xs
swapPair (a, b) (x:xs) | x == b = a:swapPair (a, b) xs
swapPair (a, b) (x:xs) = x:swapPair (a, b) xs
swapPair _ [] = []

swapPairs :: [(Integer, Integer)] -> [Integer] -> [Integer]
swapPairs pairs s = foldr swapPair s pairs

fixStatement :: Map.Map Integer [Integer] -> [Integer] -> [Integer]
fixStatement m s =
    let swapped = swapPairs (violatingPairs m s) s
    in if swapped == s then s else fixStatement m swapped
