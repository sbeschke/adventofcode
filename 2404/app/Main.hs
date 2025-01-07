module Main where

import Data.List(transpose, isPrefixOf, tails)

main :: IO ()
main = do
    content <- readFile "input"
    print $ show (process content)
    print $ show (countXmas (lines content))
    -- print $ show (replaceAllXmas (lines content))

process :: String -> Int
process content = countBlock (lines content) +
                  countBlock (map reverse $ lines content) +
                  countBlock (rows content) +
                  countBlock (map reverse $ rows content) +
                  countBlock (diagsUp (lines content)) +
                  countBlock (map reverse $ diagsUp (lines content)) +
                  countBlock (diagsDown (lines content)) +
                  countBlock (map reverse $ diagsDown (lines content))

countBlock :: [String] -> Int
countBlock ls = sum (map countLine ls)

countLine :: String -> Int
countLine line = length (filter (\s -> "XMAS" `isPrefixOf` s) (tails line))

rows :: String -> [String]
rows s = transpose (lines s)

diagsUp :: [String] -> [String]
diagsUp = step 1
    where step :: Int -> [String] -> [String]
          step n l =
            if all (== "") l then
                []
            else
                takeDiag n l:step (n+1) (restDiag n l)

diagsDown :: [String] -> [String]
diagsDown = diagsUp . reverse

takeDiag :: Int -> [String] -> String
takeDiag _ [] = []
takeDiag 0 _ = []
takeDiag n ([]:ss) = takeDiag (n-1) ss
takeDiag n ((x:_):ss) = x:takeDiag (n-1) ss


restDiag :: Int -> [String] -> [String]
restDiag _ [] = []
restDiag 0 ss = ss
restDiag n ([]:ss) = []:restDiag (n-1) ss
restDiag n ((_:xs):ss) = xs:restDiag (n-1) ss


removeCol :: [[a]] -> [[a]]
removeCol = transpose . tail . transpose


countXmas :: [String] -> Int
countXmas [] = 0
countXmas ls = sum (map detectXmas (tails ls)) + countXmas (removeCol ls)


detectXmas :: [String] -> Int
detectXmas ((b:_:c:_):(_:'A':_):(d:_:e:_):_) =
    if all (\x -> x == 'M' || x == 'S') [b, c, d, e] &&
       (b == c && d == e && b /= d || b == d && c == e && b /= c)
    then 1
    else 0
detectXmas _ = 0


replaceAllXmas :: [String] -> [[String]]
replaceAllXmas [] = []
replaceAllXmas ls = concatMap replaceXmas (tails ls) ++ replaceAllXmas (removeCol ls)


replaceXmas :: [String] -> [[String]]
replaceXmas ((b:_:c:_):(_:'A':_):(d:_:e:_):_) =
    if all (\x -> x == 'M' || x == 'S') [b, c, d, e] &&
       (b == c && d == e && b /= d || b == d && c == e && b /= c)
    then [[[b, '.', c], ['.', 'A', '.'], [d, '.', e]]]
    else []
replaceXmas _ = []
