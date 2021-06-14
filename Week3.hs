append::[a]->[a]->[a]
append [] xs =xs
append (x:xs) ys = x : append xs ys

myFlatten::[[a]]->[a]
myFlatten [x] =x
myFlatten (x:xs) =x ++ myFlatten xs

myTake::Int->[a]->[a]
myTake 0 _ =[]
myTake n (x:xs) =x : myTake (n-1) xs

mySlice::[a]->Int->Int->[a]
mySlice _ _ 0=[]
mySlice (x:xs) 1 n =x:mySlice xs 1 (n-1)
mySlice (x:xs) m n =mySlice xs (m-1) (n-1)

countOccurances::(Eq a)=>[a]->a->Int
countOccurances [] _ =0
countOccurances [x] y =0
countOccurances (x:xs) y =if(x==y) then (1+countOccurances xs y) else countOccurances xs y

remove::(Eq a)=>[a]->a->[a]
remove [] _ =[]
remove (x:xs) y =if(x==y) then remove xs y else x:remove xs y

isDescending::Int->Bool
isDescending x =if(x<10) then True else if(x `mod` 10<=(x `div` 10) `mod` 10) then isDescending (x `div` 10) else False

divisors :: Int -> [Int]
divisors y = [x | x <- [1 .. y], y `rem` x == 0 ]

prime::Int->Bool
prime x = length (divisors x) == 2

endsWith::Int->Int->Bool
endsWith x y
	|	x<10&&y>=10 =False
	|	y<10 =(x `mod` 10 == y)					
	|	otherwise =if(x `mod` 10 == y `mod` 10) then endsWith (x `div` 10) (y `div` 10) else False 
