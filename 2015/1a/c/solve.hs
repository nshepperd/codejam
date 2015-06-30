{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List (sort)
import           Data.List.Split
import           Data.Monoid
import           Data.Ratio

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = [(Integer, Integer)]

-- data Angle = Angle Int Int deriving (Show)

-- instance Eq Angle where
--   (Angle a b) == (Angle c d) =
--     (signum a, signum b) == (signum c, signum d)
--     && a * d == b * c

-- instance Ord Angle where
--   (Angle a b) <= (Angle c d) = atan2 (fromIntegral b) (fromIntegral a) <= atan2 (fromIntegral d) (fromIntegral c)

-- toAngle :: Int -> Int -> Angle
-- toAngle y x = Angle x y

-- nope :: Angle -> Angle -> Bool
-- nope (Angle x y) (Angle a b) = y * a + (-x) * b < 0

regular :: Double -> Double
regular x
  | x < 0 = x + 2*pi
  | x >= 2*pi = x - 2*pi
  | otherwise = x

divide :: [Double] -> Int
divide xs = top - go 0 0 xs (cycle xs)
  where
    top = length xs
    go m _ [] _ = m
    go m n (x:xs) (y:ys)
      | n == top             = top
      | regular (y - x) > pi = go (max m n) (n-1) xs (y:ys)
      | otherwise            = go m (n+1) (x:xs) ys

-- divide' :: [Angle] -> Int
-- divide' xs = top - go 0 0 xs (cycle xs)
--   where
--     top = length xs
--     go m _ [] _ = m
--     go m n (x:xs) (y:ys)
--       | n == top             = top
--       | nope x y             = go (max m n) (n-1) xs (y:ys)
--       | otherwise            = go m (n+1) (x:xs) ys

solveCase :: Case -> String
solveCase xs = init ("\n" <> unlines (map (show . solve) xs))
  where solve (x,y) =
          let
            disp (a,b) = atan2 (fromIntegral (b - y)) (fromIntegral (a - x))
          in divide $ sort (map disp $ filter (/=(x,y)) xs)

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
fetch (ns:rest) = let n = read ns :: Int
                      part = map (\[a,b] -> (a,b)) $ map (map read . words) $ take n rest
                  in (part, drop n rest)
