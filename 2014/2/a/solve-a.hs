import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

type Case = (Int, MultiSet Int)

solve :: Case -> Int
solve (x, items)
    | MS.null items  = 0
    -- Take the biggest item. If it's the last...
    | MS.null items' = 1
    -- Otherwise, get the biggest item that fits...
    | num > 0        = 1 + solve (x, MS.delete (x-mx) items')
    | MS.null less   = 1 + solve (x, items')
    | otherwise      = 1 + solve (x, MS.delete (MS.findMax less) items')
    where (Just (mx, items')) = MS.maxView items
          (less, num, more) = MS.splitOccur (x-mx) items'

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
fetch (as:bs:rest) = this : fetch rest
    where (n:x:[]) = map read $ words as
          items = map read $ words bs
          this = (x, MS.fromList items)
