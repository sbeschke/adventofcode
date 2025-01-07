{-# LANGUAGE LambdaCase #-}
import Data.Char
import Data.List
import System.IO


main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    putStr $ show $ compute contents
    putStr "\n"
    hClose handle


compute :: String -> Int
compute blob = sum (map lineCode (lines blob))


lineCode :: String -> Int
lineCode s = case (firstNumber s, lastNumber s) of
    (Just f, Just l) -> f * 10 + l
    _ -> -1


firstNumber :: String -> Maybe Int
firstNumber s =
    foldl aggregate Nothing (map lettersToNumber (tails s))
    where
        aggregate Nothing jy = jy
        aggregate (Just x) _ = Just x


lastNumber :: String -> Maybe Int
lastNumber s =
    foldl aggregate Nothing (map lettersToNumber (tails s))
    where
        aggregate _ (Just y) = Just y
        aggregate jx _ = jx


lettersToNumber :: String -> Maybe Int
lettersToNumber "" = Nothing
lettersToNumber s@(h:_) = 
    if isDigit h then
        Just (digitToInt h)
    else
        matchNumberString s


numberStrings :: [(String, Int)]
numberStrings = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]


matchNumberString :: String -> Maybe Int
matchNumberString s =
    case matches of
        (h:_) -> h
        _ -> Nothing
    where
        tests = map (\(n, i) -> if n `isPrefixOf` s then Just i else Nothing) numberStrings
        matches = filter (\case Just _ -> True; Nothing -> False) tests
