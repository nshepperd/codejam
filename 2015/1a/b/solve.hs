{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List.Split
import           Data.Monoid

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = (Integer, [Integer])

bisect :: (Integer -> Bool) -> Integer -> Integer -> Integer
bisect p low high
   | not (not (p low) && p high) = error "bisect: bad starting condition"
   | high == low + 1  = high
   | p mid            = bisect p low mid
   | otherwise        = bisect p mid high
   where
   mid = (low + high) `div` 2

solveCase :: Case -> String
solveCase (n, ms) = let bs = map (*10000) ms
                        -- handled t: number of people who have started being served at time t
                        handled t = sum $ zipWith (\m i -> (t - i) `div` m + if t >= i then 1 else 0) bs [0..]
                        t = bisect (\t -> handled t >= n) 0 (maximum bs * n + 10000)
                    in show (t `mod` 10000 + 1)

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
fetch (ns:ms:rest) = let part = (read (words ns !! 1), map read (words ms))
                     in (part, rest)
