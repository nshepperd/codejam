import           Data.List.Split

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = [Int]

costToReduce :: Int -> Int -> Int
costToReduce x y = let (q, r) = divMod x y
                   in (q - 1) + min 1 r

solveCase :: Case -> String
solveCase cs = show $ minimum $ map go [1 .. maximum cs]
  where go k = k + sum (map (flip costToReduce k) cs)

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
fetch (_:ds:rest) = (map read (words ds), rest)
