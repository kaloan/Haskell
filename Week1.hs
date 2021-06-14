sum2::Int->Int->Int
sum2 x y = x+y

fib::Int->Int
fib n
	| n==1 =0
	| n==2 =1
	| otherwise =fib(n-1)+fib(n-2)

fact::Int->Int
fact n
	| n==0 =1
	| n>0 =n*fact(n-1)
	|otherwise =0

length1::[Int]->Int
length1 [] =0
length1 (_:x) =1+length1 x

maxElement::[Int]->Int
maxElement [x] = x
maxElement (y:xs) = if y>maxElement xs then y else maxElement xs

myReverse::[Int]->[Int]
myReverse [] =[]
myReverse [a] =[a]
myReverse (x:xs) =myReverse xs ++ [x]

member::[Int]->Int->Bool
member [] a =False
member (x:xs) y =x==y||member xs y