import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Ord (comparing)
import Debug.Trace (traceShow)

type Preference = (Int, Int, Int)
type Case = Map Preference Int

pluspref :: Preference -> Preference -> Preference
pluspref (a, b, c) (x, y, z) = (max a x, max b y, max c z)
sumpref :: Preference -> Int
sumpref (a, b, c) = a + b + c

bump :: (Ord k) => k -> Map k Int -> Map k Int
bump k tab = Map.insert k (1 + Map.findWithDefault 0 k tab) tab

bag :: (Ord k) => [k] -> Map k Int
bag xs = bag' xs Map.empty
    where bag' [] tab = tab
          bag' (x:xs) tab = bag' xs (bump x tab)

maxtab :: Case -> Preference
maxtab tab = foldl1 pluspref $ Map.keys tab

solvecase :: Case -> Int
solvecase tab = sum $ Map.elems (solve tab)

solve :: Case -> Case
solve tab = if sumpref (maxtab tab) <= 10000 then
                tab
            else
                solve $ minimumBy (comparing (sumpref . maxtab)) [Map.delete k tab | k <- Map.keys tab]

main :: IO ()
main = do
  text <- getContents
  let cases = getCases text
  putStr $ formatCases (map (show . solvecase) cases)

formatCases :: [String] -> String
formatCases cs = unlines $ zipWith formatCase [1..] cs
formatCase :: Int -> String -> String
formatCase n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = map bag $ fetch n rest
    where (first:rest) = lines text
          n = read first

fetch :: Int -> [String] -> [[Preference]]
fetch 0 _ = []
fetch n (first:rest) = (map readpref this) : (fetch (n-1) other)
    where m = read first
          this = take m rest
          other = drop m rest

readpref :: String -> Preference
readpref line = let a:b:c:[] = map read $ words line in (a, b, c)
