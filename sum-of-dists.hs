scalarProduct :: [Double] -> [Double] -> Double
scalarProduct vec1 vec2 = foldl (+) 0 $ zipWith (*) vec1 vec2

euclideanNorm :: [Double] -> Double
euclideanNorm vec = sqrt $ scalarProduct vec vec

distance :: [Double] -> [Double] -> Double
distance vec1 vec2 = euclideanNorm $ zipWith (-) vec1 vec2

allPairs :: [a] -> [(a, a)]
allPairs l = allPairs' l [] 
	where
		allPairs' :: [a] -> [(a, a)] -> [(a, a)]
		allPairs' [] aggl = aggl
		allPairs' [x] aggl = aggl
		allPairs' (x:xs) aggl = allPairs' xs (aggl ++ [(x,y) | y <- xs])
		
completeDistance :: [[Double]] -> Double
completeDistance points = foldl (+) 0 [distance (fst pair) (snd pair) | pair <- pairs]
	where
		pairs = allPairs points
		
--Overall distance of sqrt 3
exampleList = [[0.0,0.0,0.0], [1.0,1.0,1.0]]

--Overall distance of 3 * sqrt 2
exampleList2 = [[1.0,0.0,0.0], [0.0,1.0,0.0], [0.0,0.0,1.0]]

  
