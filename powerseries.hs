import           Data.List (foldl')

iterateWithIndex :: (Eq b, Enum b) => (a -> b -> c) -> (a -> b -> a) -> a -> [c]
iterateWithIndex = iterateWithIndex' (toEnum 0)
  where
    iterateWithIndex' n f g x = f x n : iterateWithIndex' (succ n) f g (g x n)

powerSeries :: (Eq b, Enum b) => (a -> b -> c) -> b -> a -> [c]
powerSeries f n = take (fromEnum n) . iterateWithIndex f const

powerSeries' :: (Eq b, Enum b) => (a -> b -> c) -> (a -> b -> a) -> b -> a -> [c]
powerSeries' f g n = take (fromEnum n) . iterateWithIndex f g

factorial :: Integer -> Integer
factorial n = foldl' (*) 1 [1 .. n]

round' :: Integer -> Double -> Double
round' sg num = (fromIntegral . round $ num * f) / f
  where
    f = 10 ^ sg

stRound :: Double -> Double
stRound = round' 15

sine :: Double -> Double
sine x = stRound $ sum $ powerSeries' go go' 40 (x, 1)
  where
    go :: (Double, Double) -> Integer -> Double
    go (y, acc) n = y / acc
    go' :: (Double, Double) -> Integer -> (Double, Double)
    go' (y, acc) n = (negate $ x * x * y, acc * fromIntegral ((2 * n + 3) * (2 * n + 2)))

cosine :: Double -> Double
--cosine x = stRound $ sum $ powerSeries go 40 x
cosine x = stRound $ sum $ powerSeries' go go' 40 (1, 1)
  where
    go :: (Double, Double) -> Integer -> Double
    go (y, acc) n = y / acc
    go' :: (Double, Double) -> Integer -> (Double, Double)
    go' (y, acc) n = (negate $ x * x * y, acc * fromIntegral ((2 * n + 2) * (2 * n + 1)))
