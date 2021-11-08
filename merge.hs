merge::(Ord a)=>[a]->[a]->[a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =if(x<=y) then [x]++merge xs (y:ys) else [y]++merge (x:xs) ys

first_half :: [a] -> [a]
first_half = (\xs -> case xs of
            [] -> []
            xs -> take ((length xs) `div` 2 ) xs)

second_half :: [a] -> [a]
second_half = (\xs -> case xs of
            [] -> []
            xs -> drop ((length xs) `div` 2 ) xs)

mergeSort::(Ord a)=>[a]->[a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort (first_half xs) ) (mergeSort (second_half xs) )
