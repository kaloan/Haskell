import Data.Char

fibs::Int->Int
fibs 1 =1
fibs 2 =1
fibs x =fibs(x-1) + fibs(x-2)

fibsTrue::[Int]
fibsTrue =[(fibs x)|x<-[1..]]

fibsConcN::Int->String
fibsConcN 1 =show(1)
fibsConcN x =fibsConcN(x-1)++show(fibs x)

fibsRealConc::[Int]
fibsRealConc =[read(fibsConcN x)|x<-[1..]]

getNum::Int->[Int]->Int
getNum _ [] =0
getNum x (y:ys) =if(x==y) then 1+getNum x ys else getNum x ys

removeCopies::[Int]->[Int]
removeCopies [] =[]
removeCopies (x:xs) =if(member xs x) then removeCopies xs else (x:removeCopies xs)

member::[Int]->Int->Bool
member [] a =False
member (x:xs) y =x==y||member xs y

frequencyDictionary::[Int]->[Int]->[(Int,Int)]
frequencyDictionary [] _ =[]
frequencyDictionary xs ys =[(x,(getNum x ys))|x<-(removeCopies xs)]


quicksort :: (Ord a) => [(a,a)] -> [(a,a)]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, (fst y) <= (fst x)] ++ [x] ++ quicksort [y | y <- xs, (fst y) > (fst x)]

quicksortDown :: (Ord a) => [(a,a)] -> [(a,a)]
quicksortDown [] = []
quicksortDown (x:xs) = quicksortDown [y | y <- xs, (snd y) > (snd x)] ++ [x] ++ quicksortDown [y | y <- xs, (snd y) <= (snd x)]

getMin::Int->Int
getMin n= minimum [4*i*i-13*i*n+17*n*n |i<-[1..n]]

se::[Int]->[Int]->Bool
se [] _ =True
se _ [] =False
se (x:xs) (y:ys) =if(x<=y) then se xs ys else False

pythagoreanTriplets::[(Int,Int,Int)]
pythagoreanTriplets =[(x,y,z)|x<-[1..],y<-[1..],z<-[1..],x<y,y<z,x*x+y*y==z*z]

--insertSort::[Int]->Int->[Int]
--insertSort xs (length xs - 1) =xs
--insertSort xs n=insertSort (n+1)



something::Int->String
something 0 =[]
something n =foldr (++) "" [show(fibs x)|x<-[1..n]]

fibsUnlimited::[String]
fibsUnlimited =[something x| x<-[1..]]

prime::Int->Bool
prime x = (length [n | n <- [2..x] , x `mod` n == 0] ) == 1

fibsPrimed::[Int]
fibsPrimed =[y|y<-fibsTrue, prime y]
