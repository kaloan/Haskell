import           Data.List (foldl')

iterateWithIndex :: (Eq b, Enum b) => (a -> b -> a) -> (a -> b -> a) -> a -> [a]
iterateWithIndex = iterateWithIndex' (toEnum 0)
  where
    iterateWithIndex' n f g x = f x n : iterateWithIndex' (succ n) f g (g x n)

powerSeries :: (Eq b, Enum b) => (a -> b -> a) -> b -> a -> [a]
powerSeries f n = take (fromEnum n) . iterateWithIndex f const

factorial :: Integer -> Integer
factorial n = foldl' (*) 1 [1 .. n]

round' :: Integer -> Double -> Double
round' sg num = (fromIntegral . round $ num * f) / f
  where
    f = 10 ^ sg

stRound :: Double -> Double
stRound = round' 15

sine :: Double -> Double
sine x = stRound $ x * sum (powerSeries go 40 x)
  where
    go :: Double -> Integer -> Double
    --go x n = f $ (x ^ (2 * n)) / fromIntegral ((2 * n + 1) * (2 * (n + 1)))
    go x n = f $ (x ^ (2 * n)) / fromIntegral (factorial (2 * n + 1))
      where
        f = if even n then id else negate

cosine :: Double -> Double
cosine x = stRound $ sum $ powerSeries go 40 x
  where
    go :: Double -> Integer -> Double
    go x n = f $ (x ^ (2 * n)) / fromIntegral (factorial (2 * n))
      where
        f = if even n then id else negate
