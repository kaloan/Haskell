data Tree a = Empty | Node a (Tree a) (Tree a)

tree = Node 1 
        (Node 12 
            (Node 4 
                Empty 
                Empty
            ) 
            (Node 2 
                Empty 
                (Node 4 
                    Empty 
                    Empty
                )
            )
        ) 
        (Node 21 
            (Node 6 
                Empty 
                Empty
            ) 
            (Node 3 
                Empty 
                Empty
            )
        )

sumElements::Tree Integer->Integer
sumElements Empty =0
sumElements (Node now left right) =now +(sumElements left) + (sumElements right)