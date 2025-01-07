module Main where

main :: IO ()
main = do
    content <- readFile "input"
    let solution = process content
    print solution
    let solution2 = process2 content
    print solution2

process :: String -> Int
process content = length (filter isSafe lists)
    where ls = lines content
          lists = map lineToList ls

lineToList :: String -> [Int]
lineToList s = map (\x -> read x :: Int) (words s)

isSafe :: [Int] -> Bool
isSafe xs = allAscending (diffs xs) || allDescending (diffs xs)

allAscending :: [Int] -> Bool
allAscending = all (\x -> x >= 1 && x <= 3)

allDescending :: [Int] -> Bool
allDescending = all (\x -> x <= -1 && x >= -3)

process2 :: String -> Int
process2 content = length (filter isAlmostSafe lists)
    where ls = lines content
          lists = map lineToList ls

isAlmostSafe :: [Int] -> Bool
isAlmostSafe xs = tolerate isUp xs || tolerate isDown xs

neighbours :: [a] -> [(a, a)]
neighbours xs = zip xs (tail xs)

diffs :: [Int] -> [Int]
diffs xs = map (\(l, r) -> r - l) (neighbours xs)

isUp :: (Ord a, Num a) => a -> Bool
isUp x = x >= 1 && x <= 3

isDown :: (Ord a, Num a) => a -> Bool
isDown x = x <= -1 && x >= -3

tolerate :: (Int -> Bool) -> [Int] -> Bool
tolerate p xs = any (all p . diffs) (removeOne xs)

removeOne :: [Int] -> [[Int]]
removeOne = step []
    where step :: [Int] -> [Int] -> [[Int]]
          step h [] = [h]
          step h (t:ts) = (h ++ ts) : step (h ++ [t]) ts
