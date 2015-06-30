import Data.Set (Set)
import qualified Data.Set as Set

type Case = (Int, [[Int]], Int, [[Int]])

get :: Set x -> x
get = Set.findMin

solvecase :: Case -> String
solvecase (a, as, b, bs) = let sol = Set.intersection one two in
                           case Set.size sol of
                             0 -> "Volunteer cheated!"
                             1 -> show (get sol)
                             x -> "Bad magician!"
    where one = Set.fromList $ as !! (a - 1)
          two = Set.fromList $ bs !! (b - 1)

main :: IO ()
main = do
  text <- getContents
  let cases = getCases text
  putStr $ formatSolutions (map solvecase cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (x1:a1:b1:c1:d1:x2:a2:b2:c2:d2:xs) = this : fetch xs
    where this = (read x1, getgrid [a1,b1,c1,d1], read x2, getgrid [a2,b2,c2,d2])
          getgrid = map (map read . words)
