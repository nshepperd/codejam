{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Monoid
import           Data.Ratio
import           Data.Ord

import           Debug.Trace

import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = (Integer, Integer, (Integer, Integer, Integer, Integer), (Integer, Integer, Integer, Integer))

solveCase :: Case -> String
solveCase cs = show cs

main :: IO ()
main = do
  cases <- getCases <$> getContents
  putStr $ formatSolutions (map solveCase cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (chop fetch . tail . lines) text

fetch :: [String] -> (Case, [String])
fetch (nd:s1:s2:rest) = let [n, d] = map read (words nd)
                            [s0, as, cs, rs] = map read (words s1)
                            [m0, am, cm, rm] = map read (words s2)
                            part = (n, d, (s0, as, cs, rs), (m0, am, cm, rm))
                        in
                         (part, rest)
