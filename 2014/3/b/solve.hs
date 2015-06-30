data Case = Case { dianaDamage :: Integer,
                   towerDamage :: Integer,
                   gold :: [Integer],
                   hits :: [Integer] }
            deriving (Show)

div1 :: (Integral a) => a -> a -> a
div1 x y = if x `mod` y == 0 then
               x `div` y - 1
           else
               x `div` y

-- Calculate the number of "time travel credits" we gain/lose if we kill this monster
kill :: Integer -> Integer -> Integer -> Integer
kill me tower hp = if hp <= 0 then
                       -- go back in time to before the tower killed this
                       kill me tower (hp + tower) - 1
                   else
                       if hp <= tower then
                           -- Tower would kill it this turn
                           -- We need to intervene, and kill it straight away (maybe with time travel)
                           -(hp `div1` me) -- Go back in time and hit it N times, then snipe
                       else
                           -- Let the tower hit it once
                           1 + kill me tower (hp - tower)

-- We gain time travel credits for leaving a monster
leave :: Integer -> Integer -> Integer
leave tower hp = if hp > 0 then
                     hp `div1` tower + 1
                 else
                     -- already instakilled
                     0

solve :: Case -> String
solve cs = show $ getmoney 0 True 0
    where money = [[[money' i firstHit free | free <- [0..]] | firstHit <- [True, False]] | i <- [0 .. fromIntegral $ length (hits cs)]]
          getmoney i firstHit free = money !! fromIntegral i !! (if firstHit then 0 else 1) !! fromIntegral free
          money' i firstHit free
              | i == fromIntegral (length (hits cs)) = 0
              | otherwise = let hp = if firstHit then hits cs !! (fromIntegral i) else hits cs !! (fromIntegral i) - towerDamage cs
                                options = [getmoney (i+1) True (free + leave (towerDamage cs) hp)]
                                credits = free + kill (dianaDamage cs) (towerDamage cs) hp
                                options' = if credits >= 0 then
                                               -- We have enough credits to kill this monster
                                               ((gold cs !! fromIntegral i) + getmoney (i+1) False credits) : options
                                           else
                                               options
                            in maximum options'

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
    where this = Case p q (map last info) (map head info)
          (p:q:n:[]) = map read $ words line
          rest = drop (fromIntegral n) xs
          info = map (map read . words) $ take (fromIntegral n) xs
