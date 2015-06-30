import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array.IArray (Array, listArray, (!))
import Control.Monad (guard)

data Case = Case { num :: Integer,
                   p :: Integer,
                   q :: Integer,
                   r :: Integer,
                   s ::Integer }
            deriving (Show)

tr :: Case -> Integer -> Integer
tr cs i = (i * p cs + q cs) `mod` r cs + s cs

getSmall :: (Ord a) => [a] -> [a]
getSmall [] = []
getSmall xs = [minimum xs]

splitLE :: (Ord a) => a -> Set a -> Set a
splitLE e set = case Set.splitMember e set of
                  (left, True, _) -> Set.insert e left
                  (left, False, _) -> left

splitGE :: (Ord a) => a -> Set a -> Set a
splitGE e set = case Set.splitMember e set of
                  (_, True, right) -> Set.insert e right
                  (_, False, right) -> right

solve cs = let me = count 0 e - (minimum $ left ++ middle ++ right) in
           (fromIntegral me) / (fromIntegral (count 0 e)) :: Double
    where
      ts = scanl (\acc i -> acc + tr cs i) 0 [0 .. num cs - 1]
      totals :: Array Integer Integer
      totals = listArray (0, num cs) $ ts
      count a b = totals ! b - totals ! a
      e = num cs
      set = Set.fromList ts

      left = do a <- [1 .. num cs]
                -- count a b <= count 0 a
                -- totals ! b - totals ! a <= count 0 a
                -- totals ! b <= count 0 a + totals ! a
                -- count (a-1) b >= count 0 a
                -- totals ! b >= count 0 a + totals ! (a-1)
                let uppers = splitLE (count 0 a + totals ! a) $ splitGE (count 0 a + totals ! (a-1)) set
                upper <- Set.toList uppers
                let b = fromIntegral $ Set.size $ fst $ Set.split upper set
                guard (count a b <= count 0 a)
                guard (count b e <= count 0 a)
                return $ count 0 a

      right = do b <- [e - 1, e - 2 .. 0]
                 -- count a b <= count b e
                 -- totals ! b - totals ! a <= count b e
                 -- totals ! a >= totals ! b - count b e

                 -- count a (b+1) >= count b end
                 -- totals ! (b+1) - totals a >= count b end
                 -- totals ! a <= totals ! (b+1) - count b e

                 let lowers = splitGE (totals ! b - count b e) $ splitLE (totals ! (b+1) - count b e) set
                 lower <- Set.toList lowers
                 let a = fromIntegral $ Set.size $ fst $ Set.split lower set
                 guard (count a b <= count b e)
                 guard (count 0 a <= count b e)
                 return $ count b e

      middle = do a <- [0 .. num cs - 1]
                  -- count 0 (a+1) >= count a b
                  -- count 0 (a+1) >= totals b - totals a
                  -- totals b <= count 0 (a+1) + totals a

                  -- count 0 a <= count a b
                  -- count 0 a <= totals b - totals a
                  -- totals b >= count 0 a + totals a

                  let uppers = splitGE (count 0 a + totals ! a) $ splitLE (count 0 (a+1) + totals ! a) set
                  upper <- Set.toList uppers
                  let b = fromIntegral $ Set.size $ fst $ Set.split upper set
                  guard (count 0 a <= count a b)
                  guard (count b e <= count a b)
                  return $ count a b

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
fetch (line:rest) = this : fetch rest
    where (n:p:q:r:s:[]) = map read $ words line
          this = Case n p q r s
