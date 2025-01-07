module Main where

import Data.List.Split(splitOn)
import Data.Maybe (isJust)

main :: IO ()
main = do
    content <- readFile "input"
    let equations = map parseEquation (lines content)
    let operators = map findOperators equations
    let successful = filter (\(_, mo) -> isJust mo) (zip equations operators)
    let result = sum (map (\((t, _), _) -> t) successful)
    print result

    let operators2 = map findOperators2 equations
    let successful2 = filter (\(_, mo) -> isJust mo) (zip equations operators2)
    print successful2
    let result2 = sum (map (\((t, _), _) -> t) successful2)
    print result2

parseEquation :: String -> (Integer, [Integer])
parseEquation line =
    let [testValuePart, operandPart] = splitOn ": " line
        testValue = read testValuePart :: Integer
        operands = map (\x -> read x :: Integer) (splitOn " " operandPart)
    in (testValue, operands)

findOperators :: (Integer, [Integer]) -> Maybe [String]
findOperators (test, o:os) = findOperatorsLoop test os [] o
findOperators _ = Nothing

findOperatorsLoop :: Integer -> [Integer] -> [String] -> Integer -> Maybe [String]
findOperatorsLoop test (o:os) ops val =
    if val > test
        then Nothing
        else case findOperatorsLoop test os (ops ++ ["+"]) (val + o) of
                    Just result -> Just result
                    Nothing -> findOperatorsLoop test os (ops ++ ["*"]) (val * o)
findOperatorsLoop test [] ops val = if val == test then Just ops else Nothing


findOperators2 :: (Integer, [Integer]) -> Maybe [String]
findOperators2 (test, o:os) = findOperators2Loop test [o] (os) []
findOperators2 _ = Nothing

findOperators2Loop :: Integer -> [Integer] -> [Integer] -> [String] -> Maybe [String]
findOperators2Loop test h (o:os) ops =
    if evalOps h ops > test
        then Nothing
        else case findOperators2Loop test (h ++ [o]) os (ops ++ ["+"]) of
                    Just result -> Just result
                    Nothing -> case findOperators2Loop test (h ++ [o]) os (ops ++ ["*"]) of
                        Just result -> Just result
                        Nothing -> findOperators2Loop test (h ++ [o]) os (ops ++ ["||"])
findOperators2Loop test h [] ops = if evalOps h ops == test then Just ops else Nothing

evalOps :: [Integer] -> [String] -> Integer
evalOps os ops = compute os ops
    where compute (a:b:h') ("||":ops') = compute (cct a b:h') ops'
          compute (a:b:h') ("+":ops') = compute ((a+b):h') ops'
          compute (a:b:h') ("*":ops') = compute ((a*b):h') ops'
          compute (x:_) [] = x
          compute _ _ = 0

cct :: Integer -> Integer -> Integer
cct a b = read ((show a :: String) ++ (show b :: String)) :: Integer