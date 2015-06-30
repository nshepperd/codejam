{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Foldable
import           Data.List.Split
import           Data.Monoid

import           Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Case = (Integer, [Quat])

data QuatQ = O | I | J | K deriving (Show, Eq)
data Quat = Quat !Int !QuatQ deriving (Show, Eq)

instance Monoid Quat where
  mempty = Quat 1 O
  mappend (Quat a O) (Quat b c) = Quat (a*b) c
  mappend (Quat b c) (Quat a O) = Quat (a*b) c
  mappend (Quat a I) (Quat b I) = Quat (-a*b) O
  mappend (Quat a J) (Quat b J) = Quat (-a*b) O
  mappend (Quat a K) (Quat b K) = Quat (-a*b) O

  mappend (Quat a I) (Quat b J) = Quat (a*b) K
  mappend (Quat a I) (Quat b K) = Quat (-a*b) J
  mappend (Quat a J) (Quat b I) = Quat (-a*b) K
  mappend (Quat a J) (Quat b K) = Quat (a*b) I
  mappend (Quat a K) (Quat b I) = Quat (a*b) J
  mappend (Quat a K) (Quat b J) = Quat (-a*b) I

shiftl :: (Monoid a) => a -> Integer -> a
shiftl !m 0 = m
shiftl !m i = shiftl (m <> m) (i-1)

timesp :: (Monoid a) => Integer -> a -> a
timesp 0 _ = mempty
timesp n m = if even n then
               timesp (n `div` 2) (m <> m)
             else
               -- n = (2 h + 1)
               -- n m = 2 h m + m
               --     = h (2m) + m
               timesp (n `div` 2) (m <> m) <> m

-- instance Arbitrary QuatQ where
--   arbitrary = elements [O, I, J, K]

-- instance Arbitrary Quat where
--   arbitrary = Quat <$> elements [-1, 1] <*> arbitrary

decode :: Char -> Quat
decode 'i' = Quat 1 I
decode 'j' = Quat 1 J
decode 'k' = Quat 1 K

monoidCheck :: (Monoid a, Eq a) => proxy a -> (a -> a -> a -> Bool)
monoidCheck _ a b c = a <> (b <> c) == (a <> b) <> c

monoidCheck' :: (Monoid a, Eq a) => proxy a -> (a -> Bool)
monoidCheck' _ a = mempty <> a == a

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First

findM :: Quat -> Integer -> [Quat] -> [Quat] -> Maybe ([Quat], Integer)
findM target x prefix ts = firstJust [go x mempty prefix,
                                      guard (x >= 1) >> go (x-1) (fold prefix) ts,
                                      guard (x >= 2) >> go (x-2) (fold prefix <> fold ts) ts,
                                      guard (x >= 3) >> go (x-3) (fold prefix <> timesp 2 (fold ts)) ts,
                                      guard (x >= 4) >> go (x-4) (fold prefix <> timesp 3 (fold ts)) ts]
  where
    go k e []     = if e == target then Just ([], k) else Nothing
    go k e (a:as) = if e == target then Just (a:as, k) else go k (e <> a) as


solveCase :: Case -> String
solveCase (x, ts) = let cutoff = timesp x (fold ts) == foldMap decode "ijk"
                        trying = do guard cutoff
                                    (prefix, x') <- findM (decode 'i') x [] ts
                                    findM (decode 'j') x' prefix ts
                                    return True
                    in case trying of
                        Just True -> "YES"
                        _         -> "NO"


main :: IO ()
main = do
  cases <- getCases <$> getContents
  putStr $ formatSolutions (map solveCase cases)

formatSolutions :: [String] -> String
formatSolutions cs = unlines $ zipWith formatSolution [1..] cs
    where
      formatSolution :: Int -> String -> String
      formatSolution n msg = "Case #" ++ show n ++ ": " ++ msg

getCases :: String -> [Case]
getCases text = (chop fetch . tail . lines) text

fetch :: [String] -> (Case, [String])
fetch (as:bs:rest) = let [l, x] = map read (words as)
                         ms = map decode (bs)
                     in ((x, ms), rest)
