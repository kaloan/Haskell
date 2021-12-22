-- Some useful functions
ensure :: ([a] -> [b]) -> [a] -> [b]
ensure func xs  = if null xs then [] else func xs

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = map head xs : (transpose $ map tail xs)