import System.IO (openFile, IOMode (..), hGetContents)
import System.Environment (getArgs)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

type Case = (Int, Int, Int)
type Output = Maybe (Set (Int,Int))

neighbors :: Int -> Int -> (Int, Int) -> Set (Int, Int)
neighbors w h (x, y) = Set.fromList $ do
                         dx <- [-1, 0, 1]
                         dy <- [-1, 0, 1]
                         guard ((dx,dy) /= (0,0))
                         guard (0 <= x+dx && x+dx < w)
                         guard (0 <= y+dy && y+dy < h)
                         return (x+dx, y+dy)

check :: Case -> Output -> Bool
check (w, h, m) Nothing = True
check (w, h, m) (Just free) = let f = w*h - m in
                              f == Set.size reachable
    where reachable = reach (Set.singleton (0,0)) (Set.singleton (0,0))
          reach all open = case Set.minView open of
                             Just (x, rest) -> reach' all rest x
                             Nothing -> all
          reach' all open x = if Set.null $ Set.difference (neighbors w h x) free then
                                  let new = Set.difference (neighbors w h x) all in
                                  reach (Set.union all new) (Set.union open new)
                              else
                                  reach all open


main :: IO ()
main = do
  args <- getArgs
  indata <- openFile (args!!0) ReadMode >>= hGetContents
  outdata <- openFile (args!!1) ReadMode >>= hGetContents
  let cases = getCases indata
      outputs = getOutputs outdata
      checks = zipWith check cases outputs
  print $ filter (not . snd) $ zip (zip [1..] cases) checks

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (line:xs) = this : fetch xs
    where (h:w:m:[]) = map read $ words line
          this = (w, h, m)


getOutputs :: String -> [Output]
getOutputs text = map clean $ (fetchout . lines) text

clean :: Maybe [String] -> Output
clean (Just grid) = Just $ Set.fromList $ do
                      x <- [0 .. length (head grid) - 1]
                      y <- [0 .. length grid - 1]
                      if grid!!y!!x /= '*' then
                          return (x, y)
                      else
                          []
clean Nothing = Nothing

fetchout :: [String] -> [Maybe [String]]
fetchout [] = []
fetchout (line:xs) = this : fetchout rest
    where (this, rest) = fetchout' [] xs
          fetchout' sq [] = (Just sq, [])
          fetchout' sq (line:as)
              | head line == 'C' = (Just sq, (line:as))
              | line == "Impossible" = (Nothing, as)
              | otherwise = fetchout' (sq ++ [line]) as
