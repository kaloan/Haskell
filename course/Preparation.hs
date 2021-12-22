import Data.Char
--import Linear.Epsilon

--intToString::Int->[Char]
--intToString n
--	| n>=0&&n<10 = chr n
--	| otherwise = ((chr (n `mod` 10)) : intToString (n `div` 10))

--palindromeHelper :: Int -> [Char] -> Bool
--palindromeHelper x str = if( x > ( ( length str ) `div` 2) ) then True else ( ( (str!!x) == ( str!!( ( length str) - 1 - x )) ) && ( palindromeHelper (x + 1) str ) )

--palindrome::Int->Bool
--palindrome x =palindromeHelper 0 ( reverse (intToString x) )

--symHelper::Int->Int->Int


getRevDigits::Int->[Int]
getRevDigits n
        | n<10 =[n]
		| otherwise =(getRevDigits ( n `div` 10)) ++ [(n `mod` 10)]
		
makeRev::[Int]->Int
makeRev [x] =x
makeRev (x:xs) = x+ 10 * makeRev xs

symNumber::Int->Int
symNumber x= makeRev (getRevDigits x)

isPalindrome::Int->Bool
isPalindrome x =(x==symNumber x)


interposeStreams::[a]->[a]->[a]
interposeStreams (x:xs) (y:ys) =(y:x:(interposeStreams xs ys))

mergeOrderedStreams::(Ord a)=>[a]->[a]->[a]
mergeOrderedStreams (x:xs) (y:ys) =if(x<y) then (x:(mergeOrderedStreams xs (y:ys))) else (y:(mergeOrderedStreams (x:xs) ys))

checkPoints::[(Int,Int)]->Bool
checkPoints [] =True
checkPoints [x] =True
checkPoints (x:y:ys) =if(snd x<=snd y) then checkPoints(y:ys) else False

clearBadPoints::[(Int,Int)]->[(Int,Int)]
clearBadPoints [] =[]
clearBadPoints [x] =[x]
clearBadPoints (x:y:ys) =if(snd x<=snd y) then (x:(clearBadPoints (y:ys))) else (y:(clearBadPoints ys))

distance::(Int,Int)->(Int,Int)->Float
distance x y =sqrt ( ( fromIntegral (fst x) - fromIntegral (fst y))^2 + (fromIntegral(snd x) - fromIntegral (snd y))^2 )

getArea::[(Int,Int)]->Float
getArea [] =0.0
getArea [x] =0.0
getArea (x:y:ys) =(fromIntegral lesserX * fromIntegral (snd y - snd x)) + 0.5*(distance x (fst x,snd y))*(distance y (fst x, snd y)) + getArea (y:ys)
                   where lesserX =if(fst x<fst y) then (fst x) else (fst y)


nearZero::Double->Bool
nearZero a = abs a <= 1e-12
				   
sumTerms::Int->Double->Double
sumTerms n x =if(nearZero x) then 0 else (((fromIntegral((-1)^(n+1))*x))/fromIntegral n) +sumTerms (n + 1) x

naturalLog::Double->Double
naturalLog x=sumTerms 1 x

fact::Double->Double
fact 0=1.0
fact x=x*fact(x-1)

pow::Double->Double->Double
pow x 0 =1.0
pow x y =x*(pow x (y - 1))

makeListHelper::Double->Double->Double->[Double]
makeListHelper pw minus x=minus*((pow x pw) / fact pw):(makeListHelper (pw+2) (-minus) x)

makeList::Double->[Double]
makeList x=makeListHelper 0 1 x

sumN::[Double]->Int->Double
sumN _ 0 =0.0
sumN (x:xs) n=x+sumN xs (n-1)