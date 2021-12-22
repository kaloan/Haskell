myMap::(a->b)->[a]->[b]
myMap _ [] =[]
myMap func [x] =[func x]
myMap func (x:xs) =((func x) : (myMap func xs))

myFilter::(a->Bool)->[a]->[a]
myFilter _ [] =[]
myFilter func (x:xs) =if(func x) then (x:(myFilter func xs)) else myFilter func xs
--myFilter f xs =[x | x<-xs,f x]

myFoldr::(a->b->b)->b->[a]->b
myFoldr _ start [] =start
myFoldr func start (x:xs) =(x `func` (myFoldr func start xs))

myZipWith::(a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ =[]
myZipWith _ _ [] =[]
myZipWith func (x:xs) (y:ys) =((func x y):(myZipWith func xs ys))

moreLists::[a]->[a]->[[a]]
moreLists =myZipWith (\x y -> [x,y])

myTakeWhile::(a->Bool)->[a]->[a]
myTakeWhile _ [] =[]
myTakeWhile pred (x:xs) =if(pred x) then x:(myTakeWhile pred xs) else []

myDropWhile::(a->Bool)->[a]->[a]
myDropWhile _ [] =[]
myDropWhile pred (x:xs) =if(pred x) then myDropWhile pred xs else (x:xs)

strangeFunc1::[Int]->Int
strangeFunc1 xs =myFoldr (+) 0 $ myMap ((^2).(*2)) xs