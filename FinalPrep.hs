-- 2016.17
sSquares::Int->Int->Int
sSquares n k =foldr (+) 0 (take n [x*x|x<-[1..],x `mod` k == 0])

--2016.16 Not working!!
insert::[Int]->Int->[Int]
insert [] x =[x]
insert (x:xs) y =if(x>=y) then (y:(x:xs)) else x : insert (xs) y

insertSortHelper::[Int]->[Int]->[Int]
insertSortHelper xs [] =xs
insertSortHelper now (x:left) =insertSortHelper (insert now x) left

insertSort::[Int]->[Int]
insertSort x=insertSortHelper [] x

betterInsertSort::[Int]->[Int]
betterInsertSort [] =[]
betterInsertSort (x:xs) = insert (betterInsertSort xs) x

--2016.15
power2::Int->[Int]
power2 n =[2^i|i<-[1..n]]


--2016.17
pQubes::Int->Int->Int
pQubes n k =foldr (*) 1 [x*x*x|x<-[k,2*k..n*k]]


combination::Int->Int->Int
combination n m =(fact n (n- m+1) 1) `div` (fact m 1 1)

fact::Int->Int->Int->Int
fact n i product 
 |i>n =product
 |otherwise =fact n (i + 1) (product * i)
 
sevens::Int->[Int]
sevens 0 =[]
sevens 1 =[7]
--sevens k =reverse([7 * (sevens(k-1)!!0)] ++ sevens(k-1) ) Яка симетрия!!!
sevens k=[7 * (sevens(k-1)!!0)] ++ sevens(k-1) 

trueSevens::Int->[Int]
trueSevens k =reverse(sevens k)


pow::Int->Int->Int
pow x 0 =1
pow x y =x*(pow x (y - 1))

properSevens::Int->[Int]
properSevens k =map (pow 7) [1..k] 
