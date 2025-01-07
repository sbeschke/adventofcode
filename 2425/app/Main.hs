module Main where

import Data.List(partition)
import Data.List.Split(splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data SpecType = Key | Lock
    deriving (Eq, Show)

type Bins = Map.Map (Int, Int) (Set.Set [Int])

main :: IO ()
main = do
    content <- readFile "input"
    let chunks = map lines $ splitOn "\n\n" content
    let entries = map parseChunk chunks
    let (keys', locks') = partition (\(t, _) -> t == Key) entries
    let keys = map snd keys'
    let locks = map snd locks'
    print (length keys)
    print (length locks)
    let bins = mkBins locks
    let uniqueMatches = map (matchKey bins) keys
    print $ zip keys uniqueMatches
    let uniqueMatchCount = sum (map length uniqueMatches)
    print uniqueMatchCount

parseChunk :: [String] -> (SpecType, [Int])
parseChunk [] = error "empty chunk"
parseChunk (s:ss) =
    if all (== '#') s then
        (Lock, parseChunk' (map (const 0) s) (s:ss))
    else
        (Key, parseChunk' (map (const 0) s) (reverse (s:ss)))

parseChunk' :: [Int] -> [String] -> [Int]
parseChunk' = foldl (\ counts' s -> addCounts (rowCounts s) counts')

rowCounts :: String -> [Int]
rowCounts = map (\c -> if c == '#' then 1 else 0)

addCounts :: [Int] -> [Int] -> [Int]
addCounts = zipWith (+)

mkBins :: [[Int]] -> Bins
mkBins entries =
    let indices = [(x, y) | x <- [0..4], y <- [0..6]]
        bins = map (mkBin entries) indices
    in Map.fromList (zip indices bins)

mkBin :: [[Int]] -> (Int, Int) -> Set.Set [Int]
mkBin entries (x, y) = Set.fromList $ filter (\entry -> entry !! x + y <= 7) entries

matchKey :: Bins -> [Int] -> Set.Set [Int]
matchKey bins key =
    let indexedKey = zip [0..] key
        supersets = map (bins Map.!) indexedKey
    in foldl1 Set.intersection supersets
