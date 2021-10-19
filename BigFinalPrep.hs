--import Data.Tuple.All
import Data.List
import Data.Maybe

--11.9.2014
--ins x [] = [x]
--ins x (y:ys) = (x:y:ys) : (y:(ins x ys)) 

--permutations [] = []
--permutations (x:xs) = [x:ts | ts <- permutations xs ]


--permutations [] = []
--permutations (x) = [x]
--permutations (xs:x:ys) = permutations xs ++ [x] ++ permutations ys

--14.07.2015
--map ​(​head [(\couple­>fst couple + snd couple)]​) (​foldr1 (++) [[(1,2)],[(3,4)]])  ---> [3,7]
--[zip [x] [x] | x <­ [1..5]] ---> [[(1,1)],[(2,2)],[(3,3)],[(4,4)],[(5,5)]]
--map (\(x:y:z)->x:z) [[1,2,3],[2,3,1],[3,1,2]] ---> [[1,3],[2,1],[3,2]]


--10.09.2015
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = if x < y      
                       then x : (merge xs (y:ys)) 
					   else y : (merge (x:xs) ys)

getEvens = (\l -> [y |y <- l, even y])


--13.07.2018
addDefault val [] = [val]
addDefault val l  = l

--sumMinFix :: [(a -> a)] -> [a] -> a
sumMinFix fl xl =
 sum
  (map 
   (\f -> minimum
    (addDefault 0
    [ x | x <-xl, (f x) == x ])) fl)


--10.09.2018
selectList l1 l2 = if (length l1) >= (length l2) then l1 else l2

sumMaxRoots f ll =
 sum 
  (foldl selectList []
   (map (\l -> [ x | x <-l, (f x) == 0 ]) ll))

--09.07.2019


recommended basket bestFit products =
 concatMap
 (\product -> 
  if (findPrice (bestFit product) <= basketCost && (notElem (bestFit product) basket)) then [bestFit product] else [])
  basket
 where 
   findPrice product = fromJust (lookup product products) 
   basketCost = sum (map (\x -> findPrice x) basket)

products = [(1, 10), (2, 23), (3, 20), (4, 50), (5,5)]
basket = [1, 2, 4]
bestFit 1 = 2
bestFit 2 = 2
bestFit 3 = 4
bestFit 4 = 5


--10.09.2019
addIfNew x l = if not (elem x l) then (x:l) else l
 
--annotate :: [(String,[(String,String)])] -> [String -> [(String,String)]] -> [(String,[(String,String)])]
annotate db annotators =
 map
 (\(item, labels) ->
  (item, foldr addIfNew labels
    (concat (map
     (\annotator -> annotator item) annotators)))) db

db = [("scheme", [("typing", "dynamic"), ("evaluation", "strict")]),("haskell", [("typing", "static")]), ("c++", [])]
evaluation "scheme"  = [("evaluation", "strict"), ("macros", "true")]
evaluation "haskell" = [("evaluation", "lazy")]
evaluation "c++" = evaluation "scheme"
purity lang = if lang == "haskell" then [("pure", "true")] else []
   
--05.08.2020
--argMin f l = map (\item -> (item, f item)) l

--argMin f l = head (filter (\item -> (f item) == (minimum (map f l))) l)

argMin f l = foldr1 (\x y -> if f(x)<f(y) then x else y) l

sth track = map 
  (\pointPair -> abs ((snd (snd pointPair) - snd (fst pointPair)) / (fst (snd pointPair) - fst (fst pointPair))))
  (zip track (tail track))

maxSlope track = maximum
 (map 
  (\pointPair -> abs (fromIntegral (snd (snd pointPair) - snd (fst pointPair)) / fromIntegral (fst (snd pointPair) - fst (fst pointPair))))
  (zip track (tail track)))
  
easiestTrackUnder maxLen tracks = 
   argMin maxSlope (filter (\track -> fst (last track) <= maxLen) tracks)

tracks = [[(0, 900), (100, 910), (200, 925), (300, 905), (600, 950)],[(0, 1300), (100, 1305), (500, 1340), (800, 1360), (1000, 1320)],[(0, 800), (200, 830), (300, 845), (600, 880), (800, 830)]]

-- argMin maxSlope (filter (\track -> (sum (map fst track)) <= maxLen) tracks)


--16.09.2020
--recommender pl = \piece ->
-- let 
-- avgDuration artist = sum (filter (\x -> (sel1 x) == artist) pl) / length (filter (\x -> (sel1 x) == --artist) pl)
-- option1 = (foldr (\x y -> if ))) (delete piece pl))
-- option2 =  last (sortOn sel3 (filter (\x -> (avgDuration (sel1 x) < avgDuration (sel1 piece)) pl)))
-- in if not (null option1) then (option1  )
--  else if not (null option2) then (option2  )
--   else last (sortOn sel3 (filter (\x -> (sel3 x) <= (sel3 piece)) pl))

--recommender pl = \piece ->
-- let 
-- avgDuration artist = sum (filter (\x -> (sel1 x) == artist) pl) / length (filter (\x -> (sel1 x) == artist) pl)
-- option1 = last (sortOn sel3 (delete piece pl))
-- option2 =  last (sortOn sel3 (filter (\x -> (avgDuration (sel1 x)) < (avgDuration (sel1 piece))) pl))
-- in if not (null option1) then (option1  )
--  else if not (null option2) then (option2  )
--   else last (sortOn sel3 (filter (\x -> (sel3 x) <= (sel3 piece)) pl))

recommender :: [(String, String, Int)] -> (String, String, Int) -> String
recommender pl = \piece@(pAuth,pN,pLen) ->
 let 
 avgDuration artist = fromIntegral (sum [xLen | x@(xAuth,xN,xLen)<-pl, xAuth == artist]) / fromIntegral (length [xLen | x@(xAuth,xN,xLen)<-pl, xAuth == artist])
 --option1 = last [filter (\x@(xAuth,xN,xLen) -> xAuth == pAuth) (last (delete piece pl))
 --option2 = last (filter (\x@(xAuth,xN,xLen) -> (avgDuration xAuth) < (avgDuration pAuth)) pl)
 option1 = [xN | x@(xAuth,xN,xLen)<-(delete piece pl), xLen >= pLen, xAuth == pAuth]
 option2 = [xN | x@(xAuth,xN,xLen)<-pl, avgDuration xAuth < avgDuration pAuth]
 in if not (null option1) then (head option1)
  else if not (null option2) then (last option2)
   --else last (filter (\x@(xAuth,xN,xLen) -> xLen <= pLen) pl) 
   else if (null [xN | x@(xAuth,xN,xLen)<-pl, xLen > pLen]) then pN else head [xN | x@(xAuth,xN,xLen)<-pl, xLen > pLen]


--13.07.2021
data Tree a = EmptyTree | Node {
								value :: a,
								left  :: Tree a,
								right :: Tree a
						} deriving (Show,Read)

treeWords :: (Tree Char) -> [String]
treeWords EmptyTree = []
treeWords (Node v EmptyTree EmptyTree) = [[v]]
treeWords (Node v l r) = map (v:) (wl ++ wr)
  where
	wl = treeWords l
	wr = treeWords r

exampleTree = (Node 'X' (Node 'l' (Node 'a' EmptyTree EmptyTree) (Node 'b' EmptyTree EmptyTree)) (Node 'r' EmptyTree (Node 'k' EmptyTree EmptyTree)))	
	
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = lesser ++ [x] ++ greater
  where
	lesser  = filter (<=x) xs
	greater = filter (>x) xs

--Тестчета
ones = 1 : ones