import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad
import Data.List
import Data.Tuple (swap)
import Data.Ratio
import Data.Bits

type Case = [Int]

solvecase :: Case -> String
solvecase c = show c

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

fetch :: [String] -> [Case]
fetch [] = []
fetch (n:ps:xs) = this : fetch xs
    where
      this = map read (words ps)
