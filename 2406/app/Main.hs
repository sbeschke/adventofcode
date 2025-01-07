module Main where

import Data.List(intercalate)

main :: IO ()
main = do
    inp <- readFile "input"
    let initial = loadState inp
    let states = runSimulation initial
    -- putStr $ intercalate "\n\n" (map prettyState states)
    -- putStrLn ""
    print $ countXs (last states)

loadState :: String -> [String]
loadState blob =
    let raw = lines blob
        paddedLR = map (\l -> "?" ++ l ++ "?") raw
        paddingLine = map (const '?') (head paddedLR)
    in [paddingLine] ++ paddedLR ++ [paddingLine]


prettyState :: [String] -> String
prettyState = intercalate "\n"

countXs :: [String] -> Int
countXs state = sum (map (length . filter (== 'X')) state)

runSimulation :: [String] -> [[String]]
runSimulation state = allStates
    where allStates = if next == state then [state] else state:runSimulation next
          next = runStep state

runStep :: [String] -> [String]
runStep state | "" `elem` state = state
runStep state =
    let stateFromRight = runStep (removeCol state)
        stateFromBottom = transformColumn state (addCol (getCol state) stateFromRight)
    in stateFromBottom

removeCol :: [String] -> [String]
removeCol [] = []
removeCol ("":rs) = "":removeCol rs
removeCol ((_:xs):rs) = xs:removeCol rs

addCol :: [Char] -> [String] -> [String]
addCol (c:cs) (r:rs) = (c:r):addCol cs rs
addCol _ _ = []

getCol :: [String] -> [Char]
getCol ((c:_):rs) = c:getCol rs
getCol _ = []

transformColumn :: [String] -> [String] -> [String]
transformColumn (o:os) (n:ns) =
    let stateFromBottom = transformColumn os ns
    in transformLocal (o:os) (n:stateFromBottom)
transformColumn _ _ = []

transformLocal :: [String] -> [String] -> [String]
transformLocal (('#':_):('^':_):_) ((_:as'):(_:bs'):rs') = ('#':as'):('>':bs'):rs'
transformLocal (('?':_):('^':_):_) ((_:as'):(_:bs'):rs') = ('?':as'):('X':bs'):rs'
transformLocal (( _ :_):('^':_):_) ((_:as'):(_:bs'):rs') = ('^':as'):('X':bs'):rs'
transformLocal (('v':_):('#':_):_) ((_:as'):(_:bs'):rs') = ('<':as'):('#':bs'):rs'
transformLocal (('v':_):('?':_):_) ((_:as'):(_:bs'):rs') = ('X':as'):('?':bs'):rs'
transformLocal (('v':_):( _ :_):_) ((_:as'):(_:bs'):rs') = ('X':as'):('v':bs'):rs'
transformLocal (('>':'#':_):_)     ((_:_:as'):rs')       = ('v':'#':as'):rs'
transformLocal (('>':'?':_):_)     ((_:_:as'):rs')       = ('X':'?':as'):rs'
transformLocal (('>': _ :_):_)     ((_:_:as'):rs')       = ('X':'>':as'):rs'
transformLocal (('#':'<':_):_)     ((_:_:as'):rs')       = ('#':'^':as'):rs'
transformLocal (('?':'<':_):_)     ((_:_:as'):rs')       = ('?':'X':as'):rs'
transformLocal (( _ :'<':_):_)     ((_:_:as'):rs')       = ('<':'X':as'):rs'
transformLocal _ new = new


