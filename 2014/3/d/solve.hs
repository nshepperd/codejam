import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Case = Case { coins :: Map Int Int,
                   roads :: Map Int [Int] }
            deriving (Show)

solve :: Case -> String
solve cs = show cs

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
fetch (line:xs) = this : fetch rest
    where this = Case (Map.fromList (zip [1..] cs)) (toEdges $ zip [1..] js)
          n = read line
          cs = map read (take n xs)
          js = map read (take (n-1) $ drop n xs)
          rest = drop (n + n-1) xs

addEdge :: (Ord a) => (a, b) -> Map a [b] -> Map a [b]
addEdge (a, b) m = Map.insert a (b : Map.findWithDefault [] a m) m

toEdges :: [(Int, Int)] -> Map Int [Int]
toEdges [] = Map.empty
toEdges ((a,b):xs) = let sub = toEdges xs in
                     addEdge (a,b) $ addEdge (b, a) sub
