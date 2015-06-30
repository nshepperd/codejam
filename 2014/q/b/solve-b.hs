import Data.Set (Set)
import qualified Data.Set as Set
import qualified Numeric
import Data.Ratio
-- import Numeric.Digamma

readFloat :: (RealFrac a) => String -> a
readFloat = fst . head . Numeric.readFloat

type Case = (Double, Double, Double)

-- rdigamma = digamma . fromRational

solvecase :: Case -> String
solvecase (c, f, x) = show $ ((t1' n + t2 n) :: Double)
    where rate a = 2 + f * (fromIntegral a)
          t2 a = x / rate a
          tnext a = c / rate a
          t1' a = sum $ map tnext [0..a-1]
          -- t1 a = fromRational (c / f) * (rdigamma (fromIntegral n + 2/f) - rdigamma (2/f))
          n = max 0 $ ceiling (x / c - 1 - 2 / f)

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
fetch (line:xs) = this : fetch xs
    where (c:f:x:[]) = map readFloat $ words line
          this = (c, f, x)
