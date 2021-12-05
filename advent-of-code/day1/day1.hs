import System.IO  
import Control.Monad

-- 01.12.21
numberOfIncreases :: (Num a, Ord a) => [a] -> Int
numberOfIncreases l = length . filter (> 0) $ zipWith (-) (tail l) l

numberOfIncreases' :: (Num a, Ord a) => [a] -> Int
numberOfIncreases' l = length . filter (> 0) $ zipWith (-) (tail ll) ll
	where
		ll = zipWith (+) l $ zipWith (+) (tail l) (tail $ tail l)

readInt :: String -> Int
readInt = read

main = do
	contents <- readFile "input.txt"
	let depthList = map readInt . words $ contents
	--depthList <- map readInt . words $ contents
	print $ numberOfIncreases depthList
	print $ numberOfIncreases' depthList