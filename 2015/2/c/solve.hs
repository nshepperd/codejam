{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List (sort)
import           Data.List.Split
import           Data.Monoid
import           Data.Ratio

import           Debug.Trace

import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = ([String], [String], [[String]])

solveCase :: Case -> String
solveCase (s1, s2, unk) = show $ minimum $ do (sub1, sub2) <- pos unk
                                              return $ Set.size (Set.intersection
                                                                 (Set.union sub1 q1)
                                                                 (Set.union sub2 q2))

  where
    pos [] = return (Set.empty, Set.empty)
    pos (k:ks) = do (sub1, sub2) <- pos ks
                    let s = Set.fromList k
                    [(Set.union s sub1, sub2),
                     (sub1, Set.union s sub2)]
    q1 = Set.fromList s1
    q2 = Set.fromList s2




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
fetch (ns:c1:c2:rest) = let n = read ns - 2
                            part = (words c1, words c2, map words (take n rest))
                        in
                         (part, drop n rest)
