import Data.List hiding (minimum)
import Prelude hiding (minimum)
import Data.Ord (comparing)
import Debug.Trace (traceShow)
-- import Data.Function.Memoize
import Data.Maybe

import Data.Foldable

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Map (Map)
import qualified Data.Map as Map

data Problem = Problem { bound :: (Int, Int),
                         kill :: Seq Int }
             deriving (Show)

after :: Problem -> Int -> Int
after (Problem (lo, hi) kill) index = if index + 1 < Seq.length kill then
                                          Seq.index kill (index + 1) - 1
                                      else
                                          hi

before :: Problem -> Int -> Int
before (Problem (lo, hi) kill) index = if index - 1 >= 0 then
                                           Seq.index kill (index - 1) + 1
                                       else
                                           lo

width :: Problem -> Int -> Int -> Int
width prob lo hi = if hi >= lo then
                       after prob hi - before prob lo
                   else
                       0

getbest :: Problem -> Map (Int, Int) Int -> (Int, Int) -> Int
getbest prob table (s, e) = if s == e then
                                -- Singleton
                                width prob s e
                            else
                                minimum $ map (\m -> width prob s e
                                                     + (if m > s then fromJust $ Map.lookup (s,m-1) table else 0)
                                                     + (if m < e then fromJust $ Map.lookup (m+1,e) table else 0)) [s..e]

-- solve :: Problem -> Int
solve prob = let premap = construct prob (Seq.length $ kill prob)
                 ival = (0, (Seq.length $ kill prob) - 1)
             in fromJust $ Map.lookup ival premap

construct :: Problem -> Int -> Map (Int,Int) Int
construct prob 0 = Map.empty
construct prob n = let premap = construct prob (n-1)
                       addition = Map.fromList [(ival, getbest prob premap ival) | ival <- intervals n (0, (Seq.length $ kill prob) - 1)]
                   in premap `Map.union` addition

intervals :: Int -> (Int, Int) -> [(Int, Int)]
intervals n (lo, hi) = map (\s -> (s, s + n - 1)) [lo..hi - n + 1]

main :: IO ()
main = do
  text <- getContents
  let cases = getCases text
  showSolutions $ map solve cases

showSolutions :: (Show a) => [a] -> IO ()
showSolutions xs = putStr $ formatCases (map show xs)

formatCases :: [String] -> String
formatCases cs = unlines $ zipWith formatCase [1..] cs
formatCase :: Int -> String -> String
formatCase n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Problem]
getCases text = fetch n rest
    where (first:rest) = lines text
          n = read first

fetch :: Int -> [String] -> [Problem]
fetch 0 _ = []
fetch n stuff = prob : (fetch (n-1) rest)
    where (prob, rest) = readprob stuff

readprob :: [String] -> (Problem, [String])
readprob (one:two:xs) = (Problem (1, p) (Seq.fromList $ sort k), xs)
    where p = (read . head . words) one
          k = map read (words two)
