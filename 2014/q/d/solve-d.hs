import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Data.List
import Data.Ratio

import qualified Numeric
readFloat :: (RealFrac a) => String -> a
readFloat = fst . head . Numeric.readFloat

type Case = (Set Rational, Set Rational)

solvecase :: Case -> String
solvecase (ns, ks) = show (deceit ns ks) ++ " " ++ show (honest ns ks)

deceit :: Set Rational -> Set Rational -> Int
deceit ns ks = if Set.null ks then
                   0 -- game over
               else
                   case Set.lookupGT (Set.findMin ks) ns of
                     -- If I can beat his smallest block with anything,
                     -- I can put that down and say its weight is 1 - epsilon
                     Just n -> 1 + deceit (Set.delete n ns) (Set.deleteMin ks)
                     Nothing -> 0 -- give up

-- strategy is largest to smallest, I think
honest :: Set Rational -> Set Rational -> Int
honest ns ks = if Set.null ks then
                   0 -- game over
               else
                   case Set.lookupGT (Set.findMax ns) ks of
                     -- Naomi puts down largest
                     -- Ken puts down either anything that beats it, or his smallest
                     Just k -> honest (Set.deleteMax ns) (Set.delete k ks)
                     Nothing -> 1 + honest (Set.deleteMax ns) (Set.deleteMin ks)

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
fetch (a:n:k:xs) = this : fetch xs
    where naomi = Set.fromList $ map readFloat (words n)
          ken = Set.fromList $ map readFloat (words k)
          this = (naomi, ken)
