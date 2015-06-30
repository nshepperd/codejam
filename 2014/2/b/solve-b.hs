import qualified Data.List as L

type Case = [Int]

solve :: Case -> Int
solve xs = let len = length xs in
           if len <= 2 then
               0
           else
               let (mn, index) = minimum $ zip xs [0..] in
               min index (len - 1 - index) + solve (L.delete mn xs)

main :: IO ()
main = do
  cases <- fmap getCases getContents
  putStr $ formatSolutions (map (show . solve) cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (n:ns:rest) = this : fetch rest
    where
      this = map read $ words ns
