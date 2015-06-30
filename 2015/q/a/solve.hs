import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = [Int]

solveCase :: Case -> String
solveCase cs = show $ maximum $ zipWith (-) [0..] (init $ scanl (+) 0 cs)

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
getCases text = (map fetch . tail . lines) text

fetch :: String -> Case
fetch ds = let [nx, sx] = words ds in
            map (read . return) sx
