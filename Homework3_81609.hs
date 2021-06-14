--Задача 1:

packNSame::Int->a->[a]
packNSame 0 _ =[]
packNSame n x =x : packNSame (n-1) x

packHelper::(Eq a)=>Int->[a]->[[a]]
packHelper _ [] =[]
packHelper n [x]=[packNSame n x]
packHelper nNow (x:y:ys) =if(x==y) then (packHelper (nNow+1) (y:ys)) else ((packNSame nNow x):packHelper 1 (y:ys))

pack :: (Eq a) => [a] -> [[a]]
pack xs=(packHelper 1 xs)


--Задача 2:

encodeHelper::[[a]]->[(Int,a)]
encodeHelper [] =[]
encodeHelper (x:xs) =(length x,x!!0):encodeHelper xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs= encodeHelper (pack xs)


--Задача 3:

rotate :: (Enum a) => [a] -> Int -> [a]
rotate [] _ =[]
rotate xs m = if(n>0) then (drop n xs) ++ (take n xs) else (drop (length xs + n) xs) ++ (take (length xs + n) xs)
              where n=m `mod` length xs
