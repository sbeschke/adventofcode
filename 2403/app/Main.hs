module Main where

import Data.List(isPrefixOf, tails)
import Text.Regex.TDFA((=~~))

main :: IO ()
main = do
    input <- readFile "input"
    print (process input)
    print (process2 input)

process :: String -> Integer
process input = sum (muls input)
    where muls input = justs $ map readMul (tails input)

justs :: [Maybe x] -> [x]
justs ((Just x):t) = x:justs t
justs (Nothing:t) = justs t
justs [] = []

readMul :: String -> Maybe Integer
readMul s = do
    match <- (s =~~ "\\`mul\\(([0-9]+),([0-9]+)\\)") :: Maybe (String,String,String,[String])
    let digits = getDigitsFromMatch match
    let multiplied = (read (fst digits) :: Integer) * (read (snd digits) :: Integer)
    Just multiplied
    where getDigitsFromMatch (_, _, _, [l, r]) = (l, r)

process2 :: String -> Integer
process2 input = parseDo input

parseDo :: String -> Integer
parseDo "" = 0
parseDo input = 
    case readMul input of
        Just x -> x + parseDo (tail input)
        Nothing ->
            if "don't()" `isPrefixOf` input then 
                parseDont (tail input)
            else
                parseDo (tail input)

parseDont :: String -> Integer
parseDont "" = 0
parseDont input = 
    case readMul input of
        Just x -> parseDont (tail input)
        Nothing ->
            if "do()" `isPrefixOf` input then 
                parseDo (tail input)
            else
                parseDont (tail input)

