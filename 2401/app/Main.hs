module Main where

import Data.List
import System.IO


main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    putStr $ show $ compute contents
    putStr "\n"
    putStr $ show $ compute2 contents
    putStr "\n"
    hClose handle

compute :: String -> Integer
compute blob = sum differences
  where differences = map (\(a, b) -> abs (b - a)) pairs
        pairs = zip firstColumn secondColumn
        firstColumn = processColumn (fst (unzip rows))
        secondColumn = processColumn (snd (unzip rows))
        rows = map readRow (lines blob) 

processColumn :: [String] -> [Integer]
processColumn strs = sort (map (\s -> read s :: Integer) strs)

readRow :: String -> (String, String)
readRow line = readWords (words line)
    where readWords [a, b] = (a, b)

compute2 :: String -> Int
compute2 blob = sum similarities
    where rows = map readRow (lines blob)
          firstColumn = processColumn (fst (unzip rows))
          secondColumn = processColumn (snd (unzip rows))
          similarities = map (\x -> sim x secondColumn) firstColumn

sim :: Integer -> [Integer] -> Int
sim x ys = (fromIntegral x) * nocc
    where
        nocc = length (filter (\y -> y == x) ys)