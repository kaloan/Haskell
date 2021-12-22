-- A list of only ones
ones = 1 : ones

-- The list of natural numbers
nat k = k : nat  (k + 1)
--nat 1

-- The list of squares of natural numbers
galileo k = (k * k) : galileo (k + 1)
--galileo 1

-- The powers of 2
pow2 k = k : pow2 (2 * k)
--pow2 1

-- Generalised
infList k f s = (f k) : infList (s k) f s
ones' = infList 1 id id
nat' = infList 1 id succ
galileo' = infList 1 (^2) succ 
pow2' = infList 1 id (* 2)

-- A nice way to eat up your memory ;)
-- explodes at 4th position and can't calculate 5th
biApp f x = f x x
interesting = infList 2 id (biApp (^))
