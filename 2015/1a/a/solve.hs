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

type Case = [Int]

solveCase :: Case -> String
solveCase ms = show one ++ " " ++ show two
  where
    diff = zipWith (-) (tail ms) ms
    one = sum $ map (\d -> max 0 (-d)) diff
    two = let r = maximum $ map (\d -> max 0 (-d)) diff
          in sum $ map (\n -> min n r) (init ms)

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
fetch (_:ms:rest) = (map read (words ms), rest)
