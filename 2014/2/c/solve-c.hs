{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Control.Applicative
import Control.Monad (guard)
import Data.Function (on)

import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

newtype X = X Int deriving (Eq, Ord, Enum, Num, Real, Integral)
newtype Y = Y Int deriving (Eq, Ord, Enum, Num, Real, Integral)

type Block = (X, Y, X, Y)
type Case = (X, Y, [Block])
data Range a = Range { left :: !a, right :: !a}

xrange :: Block -> Range X
xrange (x0, _, x1, _) = Range x0 x1
yrange :: Block -> Range Y
yrange (_, y0, _, y1) = Range y0 y1

corners :: Block -> [(X, Y)]
corners (x0, y0, x1, y1) = liftA2 (,) [x0, x1] [y0, y1]

-- intersection of two ranges on the number line
intersects :: (Ord a) => Range a -> Range a -> Bool
intersects a b = right a >= left b && right b >= left a

chebyshev :: (X, Y) -> (X, Y) -> Int
chebyshev (a, b) (c, d) = (max `on` abs) (fromIntegral $ a - c) (fromIntegral $ b - d)

block_dist :: Block -> Block -> Int
block_dist a@(ax0, ay0, ax1, ay1) b@(bx0, by0, bx1, by1)
    | (intersects `on` xrange) a b && (intersects `on` yrange) a b = 0 -- intersecting
    | (intersects `on` xrange) a b = fromIntegral $ (min `on` abs) (ay0 - by1) (ay1 - by0)
    | (intersects `on` yrange) a b = fromIntegral $ (min `on` abs) (ax0 - bx1) (ax1 - bx0)
    | otherwise = minimum $ liftA2 chebyshev (corners a) (corners b)


left_dist :: Block -> Int
left_dist (x0, _, _, _) = fromIntegral x0

right_dist :: X -> Block -> Int
right_dist w (_, _, x1, _) = fromIntegral (w - x1)

search :: Case -> (PQueue Int Block) -> (Set Block) -> Int
search (w,h,blocks) frontier visited = case PQ.minViewWithKey frontier of
                                         Just ((d, current), frontier') -> if current == (w,0,w,h) then
                                                                               d
                                                                           else
                                                                               if Set.member current visited then
                                                                                   search (w,h,blocks) frontier' visited
                                                                               else
                                                                                   next d current frontier'
                                         Nothing -> error "should always be able to reach other side"
    where next d current frontier' = let visited' = Set.insert current visited
                                         added = do new <- (w,0,w,h):blocks
                                                    guard (Set.notMember new visited')
                                                    return (d + block_dist current new, new)
                                         frontier'' = PQ.fromList added `PQ.union` frontier' in
                                     search (w,h,blocks) frontier'' visited'

solve (w, h, blocks) = search (w,h,blocks) (PQ.singleton 0 (0,0,0,h)) Set.empty


main :: IO ()
main = do
  cases <- fmap getCases getContents
  putStr $ formatSolutions (map (show . solve) cases)
  -- putStr $ formatSolutions $ map show cases

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (fetch . tail . lines) text

fetch :: [String] -> [Case]
fetch [] = []
fetch (bounds:xs) = this : fetch rest
    where
      (w:h:b:[]) = map read $ words bounds
      blocks = map ((\(x0:y0:x1:y1:[]) -> (X x0, Y y0, X x1 + 1, Y y1 + 1)) . (map read) . words) (take b xs)
      rest = drop b xs
      this = (fromIntegral w, fromIntegral h, blocks)
