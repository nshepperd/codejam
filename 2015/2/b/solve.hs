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

import           Numeric.LinearProgramming

type Case = (Double, Double, [(Double, Double)])

solveCase :: Case -> String
solveCase (v, x, rcs) = case exact prob constr1 bound of
                         Optimal (0,_) -> "IMPOSSIBLE"
                         Optimal (f,ts) -> show (v / f)
                         _         -> "IMPOSSIBLE"
  where
    n = length rcs
    prob = Maximize (map fst rcs)
    bound = [i :&: (0, 1) | i <- [1..n]]
    constr1 = Dense [ map (\(r,c) -> (c - x) * r) rcs :==: 0 ]

-- solveCase :: Case -> String
-- solveCase (v, x, ss) = let sol = case ss of
--                              [s1] -> single s1
--                              [s1, s2] -> dual s1 s2
--                        in case sol of
--                            Just n -> show n
--                            Nothing -> "IMPOSSIBLE"
--   where
--     single (r, c) = if c == x then
--                       Just (v / r)
--                     else
--                       Nothing
--     dual (r1, c1) (r2, c2) = if min c1 c2 <= x && x <= max c1 c2 then
--                                if c1 == c2 then
--                                  Just (v / (r1 + r2))
--                                else
--                                  let t = (x - c2) / (c1 - c2)
--                                      v1 = t * v
--                                      v2 = (1 - t) * v
--                                      time = max (v1 / r1) (v2 / r2)
--                                  in Just time
--                              else
--                                Nothing

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
fetch (ds:rest) = let [n, v, x] = words ds
                      part line = let [r, c] = map read (words line) in
                                   (r, c)
                  in ((read v, read x, map part (take (read n) rest)), drop (read n) rest)
