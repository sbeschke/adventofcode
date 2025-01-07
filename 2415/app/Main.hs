module Main where

import Data.List(find, intercalate)
import Data.List.Split(splitOn)
import qualified Data.Map.Strict as Map

type State = Map.Map (Int, Int) Char

main :: IO ()
main = do
    content <- readFile "input"
    let (state, moves) = parseInput content
    let final = simulate state moves
    putStrLn (showState final)
    print (checksum final)

showState :: State -> String
showState state =
    let maxX = maximum (map (\((x, _), _) -> x) (Map.toList state))
        maxY = maximum (map (\((_, y), _) -> y) (Map.toList state))
    in intercalate "\n" [[state Map.! (x, y) | x <- [0..maxX]] | y <- [0..maxY]]

checksum :: State -> Int
checksum state =
    let relevant = filter (\(_, c) -> c == 'O') (Map.toList state)
        coords = map (\((x, y), _) -> 100 * y + x) relevant
    in sum coords

parseInput :: String -> (State, String)
parseInput content = case splitOn "\n\n" content of
                         [state, moves] -> (loadState state, filter (/= '\n') moves)
                         _ -> error "parse error"

loadState :: String -> State
loadState content = Map.fromList coordList
    where coordList = concat coordLines
          coordLines = map coordLine (zip [0..] (lines content))
          coordLine (y, l) = map (\(x, l) -> ((x, y), l)) (zip [0..] l)

simulate :: State -> String -> State
simulate = foldl simulateStep

simulateStep :: State -> Char -> State
simulateStep state '<' = simulateStep' state (-1, 0)
simulateStep state '>' = simulateStep' state (1, 0)
simulateStep state '^' = simulateStep' state (0, -1)
simulateStep state 'v' = simulateStep' state (0, 1)
simulateStep _ _ = error "invalid move"

simulateStep' :: State -> (Int, Int) -> State
simulateStep' state step =
    let pos = findPos state '@'
        lookahead = look state pos step
        shifted = push lookahead
        state' = write state pos step shifted
    in state'

look :: State -> (Int, Int) -> (Int, Int) -> String
look state position step =
    case Map.lookup position state of
        Nothing -> []
        Just c -> c:look state (posAdd position step) step

write :: State -> (Int, Int) -> (Int, Int) -> String -> State
write state _ _ [] = state
write state position step (c:cs) = write state' position' step cs
    where state' = Map.insert position c state
          position' = posAdd position step

posAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
posAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

findPos :: State -> Char -> (Int, Int)
findPos state char =
    let assocs = Map.toList state
    in case find (\(_, b) -> b == char) assocs of
           Just (pos, _) -> pos
           Nothing -> error "not found"

push :: String -> String
push = push' []
    where push' behind ('.':as) = '.':reverse behind ++ as
          push' behind ahead@('#':_) = reverse behind ++ ahead
          push' behind [] = behind
          push' behind (a:as) = push' (a:behind) as
