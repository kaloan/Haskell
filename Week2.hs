getAllTuples :: [Int] -> [(Int, Int)]
getAllTuples xs = [(x, y) | x <- xs, y <- xs]

isRightTriangle::[Int]->[(Int,Int,Int)]
isRightTriangle xs= [(x,y,z)|x<-xs,y<-xs,z<-xs,x<y,x*x+y*y==z*z]

myDrop::[Int]->Int->[Int]
myDrop xs 0 =xs
myDrop [a] 1 =[]
myDrop (x:xs) i =myDrop xs (i-1)

myZip::[Int]->[Int]->[(Int,Int)]
myZip (x:xs) (y:ys) =[(x,y)] ++ myZip xs ys
myZip [] _ =[]
myZip _ [] =[]

sumElems::[Int]->Int
sumElems [] =0
sumElems (x:xs) =x +sumElems xs

isSorted::[Int]->Bool
isSorted [] =True
isSorted [x] =True
isSorted (x:y:ys) =if(x>y) then False else isSorted (y:ys)