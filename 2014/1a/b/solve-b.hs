{- GeneralizedNewtypeDeriving -}
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (sort)
import Data.Tuple (swap)

newtype Node = Node Int deriving (Eq, Ord)
type Case = Set (Int, Int)
type Edges = Set (Node, Node)
type Pointers = Map Node [Node]

tomap :: Edges -> Pointers
tomap edges = tomap' (Set.elems edges ++ map swap (Set.elems edges)) (Map.empty)
    where tomap' [] m = m
          tomap' ((a,b):es) m = let cur = Map.findWithDefault [] a m
                                    m' = Map.insert a (b:cur) m in
                                tomap' es m'

solve :: Case -> [Int]
solve cas = do
  root <- Set.elems nodes
  return (solve' nodes efrom root)
    where
      edges = Set.map (\(a, b) -> (Node a, Node b)) cas
      nodes = Set.union (Set.map fst edges) (Set.map snd edges)
      efrom = tomap edges

solve' :: Set Node -> Pointers -> Node -> Int
solve' nodes edges root = (Set.size nodes) - clean Nothing root
    where
      clean p n = case (length $ children p n) of
                    0 -> 1 -- no children, only this
                    1 -> 1 -- we have to delete the child, only this remains
                    2 -> 1 + sum (map (clean (Just n)) (children p n)) -- may have to delete descendents
                    x -> 1 + sum (drop (x-2) $ sort (map (clean (Just n)) (children p n))) -- take two largest
      children Nothing n = Map.findWithDefault [] n edges
      children (Just p) n = filter (/=p) (Map.findWithDefault [] n edges)

solvecase :: Case -> String
solvecase edges = show $ minimum $ solve edges

main :: IO ()
main = do
  text <- getContents
  let cases = getCases text
  putStr $ formatSolutions (map solvecase cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

getbinary :: String -> [Int]
getbinary = map (read . return)

fetch :: [String] -> [Case]
fetch [] = []
fetch (ns:xs) = this : fetch rest
    where
      n = read ns
      (es, rest) = (take (n-1) xs, drop (n-1) xs)
      edges = map (\e -> let (a:b:[]) = (map read $ words e) in (a,b)) es
      this = Set.fromList edges
