{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List (sort)
import           Data.List.Split
import           Data.Monoid
import           Data.Ratio

import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- data Arrow = UP | DOWN | LEFT | RIGHT deriving (Show)
type Arrow = Char
type Case = (Int, Int, Vector (Vector Arrow))

solveCase :: Case -> String
solveCase (w, h, grid) = case sum <$> traverse go arrows of
                          Just n -> show n
                          Nothing -> "IMPOSSIBLE"
  where
    horizSets, vertSets :: Vector (Set Int)
    horizSets = V.fromList [Set.fromList
                            [x | x <- [0 .. w-1], (grid V.! y V.! x) /= '.']
                           | y <- [0 .. h - 1]]

    vertSets = V.fromList [Set.fromList
                           [y | y <- [0 .. h-1], (grid V.! y V.! x) /= '.']
                          | x <- [0 .. w - 1]]

    arrows = do y <- [0 .. h - 1]
                x <- [0 .. w - 1]
                let c = grid V.! y V.! x
                guard (c /= '.')
                return (x, y, c)

    findFix x y = if Set.size (vertSets V.! x) > 1
                     || Set.size (horizSets V.! y) > 1 then
                    Just 1
                  else Nothing

    go :: (Int, Int, Char) -> Maybe Int
    go (x, y, '>') = case Set.lookupGT x (horizSets V.! y) of
                      Just _ -> Just 0
                      Nothing -> findFix x y

    go (x, y, '<') = case Set.lookupLT x (horizSets V.! y) of
                      Just _ -> Just 0
                      Nothing -> findFix x y

    go (x, y, '^') = case Set.lookupLT y (vertSets V.! x) of
                      Just _ -> Just 0
                      Nothing -> findFix x y

    go (x, y, 'v') = case Set.lookupGT y (vertSets V.! x) of
                      Just _ -> Just 0
                      Nothing -> findFix x y


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
fetch (ds:rest) = let [r, c] = map read (words ds) :: [Int]
                      part line = V.fromList line
                  in ((c, r, V.fromList $ map part (take r rest)), drop r rest)
