import Data.Ratio
import Text.ParserCombinators.Parsec
type Case = Ratio Integer

bits :: (RealFrac a) => Int -> a -> Maybe String
bits 0 r = if r == 0 then Just [] else Nothing
bits n r = if r >= 1/2 then
               fmap ('1':) (bits (n-1) (r*2 - 1))
           else
               fmap ('0':) (bits (n-1) (r*2))

solve :: Case -> Maybe Int
solve r = if r == 1 then
              return 0
          else
              fmap ((1+) . length . (takeWhile (=='0'))) (bits 40 r)

solvecase :: Case -> String
solvecase c = case solve c of
                Just n -> show n
                Nothing -> "impossible"

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

split :: (Eq a) => a -> [a] -> [[a]]
split x [] = [[]]
split x (y:ys) = if x == y then
                     [] : (split x ys)
                 else
                     let (a:as) = split x ys in
                     (y:a):as

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (line:rest) = this : fetch rest
    where this = (read num) % (read den)
          (num:den:[]) = split '/' line
