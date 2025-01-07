module Main where

import Data.List(findIndex, transpose)
import Data.List.Extra((!?), minimumOn)
import qualified Data.Map.Strict as Map
import GHC.Base (maxInt)

-- Node = Position x Direction
type Node = ((Int, Int), Char)
type Costs = Map.Map Node Int

main :: IO ()
main = do
    content <- readFile "input"
    let themap = lines content
    let costs = simulate themap
    print $ readScore themap costs

initialNode :: [String] -> Node
initialNode themap = (findPos 'S' themap, '>')

finalPos :: [String] -> (Int, Int)
finalPos = findPos 'E'

findPos :: Char -> [String] -> (Int, Int)
findPos c ss =
    let my = findIndex (elem c) ss
        mx = findIndex (elem c) (transpose ss)
    in case (mx, my) of
        (Just x, Just y) -> (x, y)
        _ -> error "ow"

indexMap :: [String] -> [((Int, Int), Char)]
indexMap ss = concatMap (\(y, r) -> indexRow y r) (zip [0..] ss)
    where indexRow y r = map (\(x, c) -> ((x, y), c)) (zip [0..] r)

allNodes :: [String] -> [Node]
allNodes ss =
    let indexed = indexMap ss
        valid = filter (\(_, c) -> c /= '#') indexed
        coords = map (\((x, y), _) -> (x, y)) valid
    in concatMap (\c -> [(c, '^'), (c, '>'), (c, 'v'), (c, '<')]) coords

initialCosts :: Node -> Costs
initialCosts start = Map.fromList [(start, 0)]

readScore :: [String] -> Costs -> Int
readScore themap costs =
    let pos = finalPos themap
        finalNodes = [(pos, '>'), (pos, '^'), (pos, '<'), (pos, 'v')]
        scores = map (nodeCost costs) finalNodes
    in minimum scores

transitions :: [String] -> Node -> [(Node, Int)]
transitions themap node = filter (\((pos, _), _) -> isValidPos themap pos) (transitions' node)

transitions' :: Node -> [(Node, Int)]
transitions' ((x, y), '>') = [(((x+1, y), '>'), 1), (((x, y), '^'), 1000), (((x, y), 'v'), 1000)]
transitions' ((x, y), 'v') = [(((x, y+1), 'v'), 1), (((x, y), '>'), 1000), (((x, y), '<'), 1000)]
transitions' ((x, y), '<') = [(((x-1, y), '<'), 1), (((x, y), 'v'), 1000), (((x, y), '^'), 1000)]
transitions' ((x, y), '^') = [(((x, y-1), '^'), 1), (((x, y), '<'), 1000), (((x, y), '>'), 1000)]
transitions' _ = error "invalid state"

isValidPos :: [String] -> (Int, Int) -> Bool
isValidPos themap (x, y) =
    case atPos of
        Just c -> c /= '#'
        Nothing -> False
    where atPos =
            do l <- themap !? y
               l !? x

nodeCost :: Costs -> Node -> Int
nodeCost costs node = Map.findWithDefault maxInt node costs

updateCost :: Costs -> Node -> Int -> Costs
updateCost costs neighbour ncost =
    let curcost = nodeCost costs neighbour
    in if curcost <= ncost then costs else Map.insert neighbour ncost costs

visitNext :: [String] -> (Costs, [Node]) -> (Costs, [Node])
visitNext themap (costs, unvisited) =
    let next = minimumOn (nodeCost costs) unvisited
        nextCost = nodeCost costs next
        costs' = foldl (\acc (neighbour, tcost) -> updateCost acc neighbour (nextCost + tcost)) costs (transitions themap next)
        unvisited' = filter (/= next) unvisited
    in (costs', unvisited')

simulate :: [String] -> Costs
simulate themap =
    let initNode = initialNode themap
        initCosts = initialCosts initNode
        initUnvisited = allNodes themap
        (finalCosts, _) = simulate' themap (initCosts, initUnvisited)
    in finalCosts

simulate' :: [String] -> (Costs, [Node]) -> (Costs, [Node])
simulate' _ (costs, []) = (costs, [])
simulate' themap (costs, unvisited) = simulate' themap (visitNext themap (costs, unvisited))
