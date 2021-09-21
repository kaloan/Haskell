--PracticeInterview Preparation KitSearchHash Tables: Ice Cream Parlor

import Data.List

getDZip cost money = dZip
	where
        tailer [] = []
        tailer (x:xs) = [(x,y)|y<-xs] ++ tailer xs
        dZip = Data.List.zip (tailer [1..Data.List.length cost]) (tailer cost)
		
getFinder cost money = finder
	where
        tailer [] = []
        tailer (x:xs) = [(x,y)|y<-xs] ++ tailer xs
        dZip = Data.List.zip (tailer [1..Data.List.length cost]) (tailer cost)
        finder = [(i,j) | x@((i,j),(c1,c2))<-dZip, c1+c2 == money]

{-
whatFlavors cost money = do
    putStr $ show ( fst res ) ++ " "
    putStr $ show ( snd res ) ++ "\n"
    where
        tailer [] = []
        tailer (x:xs) = [(x,y)|y<-xs] ++ tailer xs
        dZip = Data.List.zip (tailer [1..Data.List.length cost]) (tailer cost)
        finder = [(i,j) | x@((i,j),(c1,c2))<-dZip, c1+c2 == money]
        res=finder!!0
	
		
whatFlavors cost money = do
    putStr $ show ( fst res ) ++ " "
    putStr $ show ( snd res ) ++ "\n"
    where
		realList = zip [1..length cost] cost
		--tailer _ _ [] = []
		--tailer _ _ [a] = []
        --tailer i j (x:xs) = map (\y -> ((i,j),x,y))[(x,y)|y<-xs] ++ tailer i-1 j-1 xs
		tailer [] = []
		tailer (x:xs) = [(x,y)|y<-xs] ++ tailer xs
        finder = [(i,j) | x@((i,c1),(j,c2))<-(tailer realList), c1+c2 == money]
        res=finder!!0
-}	
money1 = 8	
cost1 = [4, 3, 2, 5, 7]
money2 = 12	
cost2 = [7, 2, 5, 4, 11]

ttt [] = []
ttt (x:xs) = [(x,y)|y<-xs] ++ ttt xs
--fd cc mm = [(fst y1, fst y2) | x@(y1,y2)<-(ttt $ zip [1..length cc] cc), (snd y1) + (snd y2) == mm]
--fd cc mm = [(i,j) | x@((i,c1),(j,c2))<-(ttt $ zip [1..length cc] cc), c1+c2 == mm]

--ttt2 :: [(Int,Integer)] -> Int -> [(Int,Int)]
ttt2 [] _ = []
ttt2 (x@(i,c1):xs) m = [(i,j)|y@(j,c2)<-xs, c1+c2 == m] ++ (ttt2 xs m)
--fd cc mm = [(i,j) | x@((i,c1),(j,c2))<-(ttt $ zip [1..length cc] cc), c1+c2 == mm]

whatFlavors cost money = do
    putStr $ show ( fst res ) ++ " "
    putStr $ show ( snd res ) ++ "\n"
	where
		--res = (fd cost money)!!0
		res = (ttt2 (zip [1..length cost] cost) money)!!0
		
		
		
		
--PracticeFunctional ProgrammingRecursionPascal's Triangle

pascal n =
    take n (iterate 
        (\x -> [1] ++ (zipWith (+) x (tail x)) ++ [1]) [1])
         
printPascal n =
    mapM_ putStrLn $ map (concatMap (\x -> show x ++ " ")) (pascal n)

main = do
    input <- getLine
    printPascal . (read :: String -> Int) $ input
	
	
	
	
--PracticeFunctional ProgrammingRecursionString-o-Permute

swapper  [] = []
swapper [x] = [x]
swapper (x:y:xs) = (y:x:(swapper xs)) 

calcNumTimes 0 = return ()
calcNumTimes n = do
    str <- getLine
    putStrLn $ swapper str
    calcNumTimes (n-1) 


main = do
    num <- getLine
    calcNumTimes $ read num




--PracticeFunctional ProgrammingRecursionString Compression

compress :: String -> String
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | reps == 1 = [x] ++ compress (y:xs)
    | otherwise = [x] ++ (show reps) ++ compress (drop reps (x:y:xs))
    where
        compressHelper :: String -> Int -> Int
        compressHelper [] res = res
        compressHelper [x] res = res
        compressHelper (a:b:as) res = if a==b then compressHelper (b:as) (res+1) else res
        reps = (compressHelper (x:y:xs) 1)
    
main = do
    str <- getLine
    putStrLn $ compress str
	
	
	
--PracticeFunctional ProgrammingRecursionString Reductions	
	
reduce :: String -> String
reduce [] = []
reduce (x:xs) = [x] ++ (reduce (filter (\y -> y/=x) xs))
    
main = do
    str <- getLine
    putStrLn $ reduce str
	
	
	
--PracticeFunctional ProgrammingRecursionThe Sums of Powers

flNthRoot x n = flNthRoot' x n 0
flNthRoot' 0 _ _ = 0
flNthRoot' x 1 _ = x
flNthRoot' x n y
    |y^n == x = y
    |y ^ n > x  = y-1
    |otherwise = flNthRoot' x n (y+1)

subset [] = [[]]
subset (x:xs) = (map (x:) (subset xs)) ++ (subset xs)

numOfPow x n =
    length [l | l<-subset [1..flNthRoot x n], sum(map (\t -> t^n) l) == x]
main = do
    input1 <- getLine
    input2 <- getLine
    let x = (read input1 :: Int)
    let n = (read input2 :: Int)
    putStrLn $ show $ numOfPow x n
	
	
	
	
--PracticeFunctional ProgrammingRecursionFilter Elements
import Data.List.Split

dualFilter p [] = ([],[])
dualFilter p (x:xs)
    |p x = (x:(fst rest), snd rest)
    |otherwise = (fst rest, x:(snd rest))
    where rest = dualFilter p xs

		
fltr::[Int] -> Int -> [Int] -> [Int]
fltr [] k res = res
fltr (x:xs) k res
    |reps >= k = fltr rest k (res ++ [x])
    |otherwise = fltr rest k res
    where
		dFiltered = dualFilter (\y -> y==x) (x:xs)
        reps = length $ fst dFiltered
        rest = snd dFiltered
       {- reps = length $ filter (\y -> y==x) (x:xs)
        rest = filter (\y -> not $ y==x) xs -}

fltr2 xs k res 
    |usual == [] = [-1] 
    |otherwise = usual
    where usual = fltr xs k res
 
calcNumTimes 0 = return ()
calcNumTimes n = do
    str <- getLine
    let argsList = splitOn " " str
    let n = (read (argsList!!0) :: Int)
    let k = (read (argsList!!1) :: Int)
    str2 <- getLine
    let ls = map read (splitOn " " str2)
    putStrLn $ concatMap (\x->show x ++ " ") (fltr2 ls k [])
    calcNumTimes (n-1) 


main = do
    num <- getLine
    calcNumTimes $ read num
	
	
	
--PracticeFunctional ProgrammingMemoization and DPNumber of Binary Search Tree	
	
import Control.Monad
catalan n = 
    catalanSum n 1 [1]
    where 
        catalanSum 0 a l = a
        catalanSum k a l = catalanSum (k-1) t (l ++ [t])
            where t = (sum $ zipWith (*) l (reverse l)) `mod` (10^8+7)

main = do
    input <- readLn
    replicateM_ input (readLn >>= print . catalan)