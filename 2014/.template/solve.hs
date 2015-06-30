data Case = Case

solve :: Case -> String
solve cs = ":)"

main :: IO ()
main = do
  cases <- fmap getCases getContents
  putStr $ formatSolutions (map solve cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (line:rest) = this : fetch rest
    where this = Case
