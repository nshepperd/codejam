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

type Case = (Double, [(Double, Double)])

optimalroute :: Double -> [(Double, Double)] -> [(Double, Double, Double, Double)]
optimalroute v [] = []
optimalroute v ((p,s):rs) = minimumBy (comparing (\path -> let (x, t, _, _) = last path in
                                                 t)) tries

  where
    sub = optimalroute v rs
    n = length sub

    insertAt x t p s [] = let t1 = (p - x + signum p * t * v) / (signum p * (v - s))
                              x1 = project p s t1 in
                           [(x1, t1, p, s)]
    insertAt x t p s ((x2, t2, p2, s2) : rest)
      = let t1 = (p - x + signum p * t * v) / (signum p * (v - s))
            x1 = project p s t1 in
         if signum p * project p s t2 <= signum p * x2 then
           -- reached already
           [(x1, t1, p, s)] ++ ((x2, t2, p2, s2) : rest)
         else
           [(x1, t1, p, s)] ++ insertAt x1 t1 p2 s2 rest

    tries = [try i | i <- [0..n]]

    try i = let before = take i sub
                after = drop i sub
                (x, t) = if null before then
                           (0, 0)
                         else
                           let (a, b, c, d) = last before in (a, b)
            in before ++ insertAt x t p s after

    -- cs = crosses sub p s

    project p s t = p + signum p * s * t
    -- crosses route p s = if p > 0 then
    --                       map (\(x, t) -> project p s t <= x) route
    --                     else
    --                       map (\(x, t) -> project p s t >= x) route


solveCase :: Case -> String
solveCase (v, pss) = show $ if null route then
                               0.0
                            else
                              let (x, t, _, _) = last route
                              in t
  where
    route = optimalroute v pss

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
fetch (yn:ps:ss:rest) = let [y, n] = map read (words yn)
                            ip = map read (words ps)
                            is = map read (words ss)
                            part = (y, zip ip is)
                        in
                         (part, rest)
