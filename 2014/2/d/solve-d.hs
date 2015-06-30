import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Math.Combinatorics.Exact.Binomial (choose)

type Value = Integer
data Trie a = Node !a !Bool !(Map Char (Trie a))
              deriving (Show)
type Case = (Value, [String])

emptytrie :: (Num a) => Trie a
emptytrie = Node 0 False (Map.empty)

addstr :: (Num a) => String -> Trie a -> Trie a
addstr [] (Node n l ch) = Node (n+1) True ch
addstr (c:cs) (Node n l ch) = let child = Map.findWithDefault emptytrie c ch
                                  newchild = addstr cs child in
                              seq newchild $ Node (n+1) l (Map.insert c newchild ch)

totrie :: (Num a) => [String] -> Trie a
totrie strings = foldl' (\trie s -> addstr s trie) emptytrie strings

cover :: (Integral a) => a -> [a] -> a
cover servers ns = getcover servers len
    where cover' 0 0 = 1
          cover' left 0 = 0
          cover' 0    i = product $ map (choose servers . (min servers)) (drop (len - i) ns)
          cover' left i = let n = ns !! (len - i) in
                          if n < servers then
                              let notleft = servers - left in
                              sum $ do used <- [max 0 (n - notleft)  .. min n left]
                                       let mul = remod (choose left used) * remod (choose notleft (n - used))
                                       return $ mul * getcover (left - used) (i-1)
                          else
                              getcover 0 (i-1)
          coverlist = [[remod $ cover' left i | left <- [0..servers]] | i <- [0 .. length ns]]
          getcover left i = coverlist !! (fromIntegral i) !! (fromIntegral left)
          len = length ns

-- Numbers modulo 1000000007
remod :: (Integral a) => a -> a
remod = (`mod` 1000000007)

getLeaves :: Trie a -> a
getLeaves (Node leaves _ _) = leaves

count :: (Integral a) => a -> Trie a -> (a, a) -> (a, a)
count 1 (Node 1 True _) (add, mul) = (add + 1, mul)
count 1 (Node 1 False children) (add, mul) = count 1 (snd $ Map.findMin children) (add + 1, mul)
count servers (Node leaves isleaf children) (add, mul) = let ns = map getLeaves $ Map.elems children
                                                             ns' = if isleaf then 1:ns else ns in
                                                         foldl' acc (remod $ add + min servers leaves, remod $ mul * cover servers ns') children

    where acc (add, mul) node = count (min servers (getLeaves node)) node (add, mul)

solve :: Case -> (Value, Value)
solve (n, strings) = let trie = totrie strings in
                     count n trie (0, 1)

main :: IO ()
main = do
  cases <- fmap getCases getContents
  putStr $ formatSolutions (map ((\(a, b) -> show a ++ " " ++ show b) . solve) cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (nums:xs) = this : fetch rest
    where
      (m:n:[]) = map read $ words nums
      strings = take m xs
      rest = drop m xs
      this = (fromIntegral n, strings)
