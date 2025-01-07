{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Data.List.Split(splitOn)
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf, sort, sortOn, find, intercalate)

type State = Map.Map String Int
type Adjacencies = Map.Map String [String]

main :: IO ()
main = do
    content <- readFile "input"
    let [inputs', connections'] = splitOn "\n\n" content
    let state0 = parseInputs (lines inputs')
    let connections = map parseConnection (lines connections')
    print state0
    print connections
    let inbound = inboundAdjacencies connections
    print inbound
    let outbound = outboundAdjacencies connections
    print outbound
    let initial = Map.keys state0
    let terminal = sort $ filter (isPrefixOf "z") (map (\(_, _, _, s) -> s) connections)
    let nodeOrder = zip (bfs inbound outbound [] initial) [0..]
    print nodeOrder
    let sortedConnections = sortOn (\(_, _, _, t) -> lookup t nodeOrder) connections
    print sortedConnections
    let finalState = foldl step state0 sortedConnections
    print finalState
    let decoded = decode finalState terminal
    print decoded
    let terms = map (normalize . decodeTerm sortedConnections) terminal
    putStrLn $ intercalate "\n\n" (map (\t -> show t :: String) terms)
    let swaps = [("qnw", "z15"), ("cqr", "z20"), ("vkg", "z37"), ("ncd", "nfj")]
    let fixed = swapConnections sortedConnections swaps
    let fixedTerms = map (normalize . decodeTerm fixed) terminal
    putStrLn $ intercalate "\n\n" (map (\t -> show t :: String) fixedTerms)
    let sortedSwaps = sort (map fst swaps ++ map snd swaps)
    putStrLn $ intercalate "," sortedSwaps


parseInputs :: [String] -> State
parseInputs ls = Map.fromList $ map parseInput ls

parseInput :: String -> (String, Int)
parseInput line =
    let [ident, val'] = splitOn ": " line
        val = fromIntegral (read val' :: Integer)
    in (ident, val)

parseConnection :: String -> (String, String, String, String)
parseConnection line =
    let [l, r] = splitOn " -> " line
        [a, op, b] = splitOn " " l
    in (a, op, b, r)

outboundAdjacencies :: [(String, String, String, String)] -> Adjacencies
outboundAdjacencies = foldl addOutboundAdjacency Map.empty

addOutboundAdjacency :: Adjacencies -> (String, String, String, String) -> Adjacencies
addOutboundAdjacency adjs (a, _, b, x) =
    let aInserted = Map.insert a (x:getAdjacencies adjs a) adjs
        bInserted = Map.insert b (x:getAdjacencies aInserted b) aInserted
    in bInserted

inboundAdjacencies :: [(String, String, String, String)] -> Adjacencies
inboundAdjacencies = foldl addInboundAdjacency Map.empty

addInboundAdjacency :: Adjacencies -> (String, String, String, String) -> Adjacencies
addInboundAdjacency adjs (a, _, b, x) = Map.insert x (a:b:getAdjacencies adjs x) adjs

getAdjacencies :: Adjacencies -> String -> [String]
getAdjacencies adjs k = Map.findWithDefault [] k adjs

bfs :: Adjacencies -> Adjacencies -> [String] -> [String] -> [String]
bfs inbound outbound visited (todo:todos) =
    let candidates = Map.findWithDefault [] todo outbound
        ready = filter (all (`elem` visited') . getAdjacencies inbound) candidates
        todos' = todos ++ ready
        visited' = if todo `elem` visited then visited else visited ++ [todo]
    in bfs inbound outbound visited' todos'
bfs _ _ visited [] = visited

step :: State -> (String, String, String, String) -> State
step state (a, op, b, x) = Map.insert x (applyOp (getState state a) op (getState state b)) state

getState :: State -> String -> Int
getState state k = case Map.lookup k state of
                       Just v -> v
                       Nothing -> error k

applyOp :: Int -> String -> Int -> Int
applyOp a "AND" b = if a == 1 && b == 1 then 1 else 0
applyOp a "OR" b = if a == 1 || b == 1 then 1 else 0
applyOp a "XOR" b = if a /= b then 1 else 0
applyOp _ _ _ = error "invalid op"

decode :: State -> [String] -> Int
decode state (t:ts) = state Map.! t + 2 * decode state ts
decode _ [] = 0


data Term = Input String
          | And String Term Term
          | Or String Term Term
          | Xor String Term Term

label :: Term -> String
label (Input l) = l
label (And l _ _) = l
label (Or l _ _) = l
label (Xor l _ _) = l

decodeTerm :: [(String, String, String, String)] -> String -> Term
decodeTerm connections lab =
    case find (\(_, _, _, l) -> l == lab) connections of
        Just operation -> decodeBinaryTerm connections operation
        Nothing -> Input lab

decodeBinaryTerm :: [(String, String, String, String)] -> (String, String, String, String) -> Term
decodeBinaryTerm conn (a, "AND", b, lab) = And lab (decodeTerm conn a) (decodeTerm conn b)
decodeBinaryTerm conn (a, "OR", b, lab) = Or lab (decodeTerm conn a) (decodeTerm conn b)
decodeBinaryTerm conn (a, "XOR", b, lab) = Xor lab (decodeTerm conn a) (decodeTerm conn b)
decodeBinaryTerm _ (_, op, _, _) = error op

normalize :: Term -> Term
normalize (And lab a b) = case compare a' b' of
                              LT -> And lab a' b'
                              EQ -> And lab a' b'
                              GT -> And lab b' a'
    where a' = normalize a
          b' = normalize b
normalize (Or lab a b) = case compare a' b' of
                             LT -> Or lab a' b'
                             EQ -> Or lab a' b'
                             GT -> Or lab b' a'
    where a' = normalize a
          b' = normalize b
normalize (Xor lab a b) = case compare a' b' of
                              LT -> Xor lab a' b'
                              EQ -> Xor lab a' b'
                              GT -> Xor lab b' a'
    where a' = normalize a
          b' = normalize b
normalize t = t


instance Show Term where
    show (And lab a b) = lab ++ "<-And(" ++ show a ++ ", " ++ show b ++ ")"
    show (Or lab a b) = lab ++ "<-Or(" ++ show a ++ ", " ++ show b ++ ")"
    show (Xor lab a b) = lab ++ "<-Xor(" ++ show a ++ ", " ++ show b ++ ")"
    show (Input lab) = lab

instance Eq Term where
    (==) (Input x) (Input y) = x == y
    (==) (And _ a b) (And _ a' b') = a == a' && b == b'
    (==) (Or _ a b) (Or _ a' b') = a == a' && b == b'
    (==) (Xor _ a b) (Xor _ a' b') = a == a' && b == b'
    (==) _ _ = False

instance Ord Term where
    -- Input < Xor < And < Or
    compare (Input a) (Input a') = compare a a'
    compare (Input _) _ = LT
    compare _ (Input _) = GT
    compare (Xor _ a _) (Xor _ a' _) = compare a a'
    compare (Xor {}) _ = LT
    compare _ (Xor {}) = GT
    compare (And _ a _) (And _ a' _) = compare a a'
    compare (And {}) _ = LT
    compare _ (And {}) = GT
    compare (Or _ a _) (Or _ a' _) = compare a a'

-- z_i = Xor(Xor(x_i, y_i), c_{i-1})
-- c_i = Or(And(Xor(x_{i-1}, y_{i-1}), c_{i-1}), And(x_{i-1}, y_{i-1}))

swapConnections :: [(String, String, String, String)] -> [(String, String)] -> [(String, String, String, String)]
swapConnections = foldl (\conns' (a, b) -> map (swapConnections' (a, b)) conns')

swapConnections' :: (String, String) -> (String, String, String, String) -> (String, String, String, String)
swapConnections' (x1, x2) (a, op, b, x) | x == x1 = (a, op, b, x2)
swapConnections' (x1, x2) (a, op, b, x) | x == x2 = (a, op, b, x1)
swapConnections' _ c = c
