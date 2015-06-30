import Control.Monad
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
type Case = (Int, Int, Int)

type Solution = Set (Int, Int)
getmap :: Solution -> Set (Int, Int)
getmap = id
soget :: (Int, Int) -> Solution -> Bool
soget k = Set.member k . getmap
soswap :: Solution -> Solution
soswap = Set.map swap
sosize :: Solution -> Int
sosize = Set.size . getmap

showsolution :: Int -> Int -> Solution -> String
showsolution w h xs = intercalate "\n" $ map (\y -> map (flip getcell y) [0..w-1]) [0..h-1]
    where getcell 0 0 = 'c'
          getcell x y = if soget (x,y) xs then
                            '.'
                        else
                            '*'

-- in solve, assume width is smaller than height
solve :: Int -> Int -> Int -> Maybe Solution
solve w h f = if w == 1 || f == 1 then
                  greedy w h f
              else
                  msum $ [greedy r h f | r <- [2..w]] ++ [almostgreedy r h f | r <- [2..w]]

greedy :: Int -> Int -> Int -> Maybe Solution
greedy w h f = let numrows = f `div` w
                   spillover = f `mod` w in
               if (f==1) || (w*h >= f && numrows >= 2 && spillover /= 1) then
                   Just $ Set.fromList $ [(x, y) | x <- [0..w-1], y <- [0..numrows-1]] ++ [(x, numrows) | x <- [0..spillover-1]]
               else
                   Nothing

almostgreedy :: Int -> Int -> Int -> Maybe Solution
almostgreedy w h f = let numrows = f `div` w
                         spillover = f `mod` w in
                     if (w*h >= f && numrows >= 3 && spillover == 1 && w > 2) then
                         Just $ Set.fromList ([(x, y) | x <- [0..w-1], y <- [0..numrows-2]]
                                              ++ [(x, numrows-1) | x <- [0..w-2]]
                                              ++ [(x, numrows) | x <- [0..spillover]])
                     else
                         Nothing

solvecase :: Case -> String
solvecase (w, h, m) = case solution of
                        Just cells -> showsolution w h cells
                        Nothing -> "Impossible"
    where f = w*h - m
          solution = if w <= h then
                         solve w h f
                     else
                         fmap soswap $ solve h w f

main :: IO ()
main = do
  text <- getContents
  let cases = getCases text
  putStr $ formatSolutions (map solvecase cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ":\n" ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (line:xs) = this : fetch xs
    where (h:w:m:[]) = map read $ words line
          this = (w, h, m)
