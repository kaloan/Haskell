{-# LANGUAGE BlockArguments #-}

-- 01.12.21
numberOfIncreases :: (Num a, Ord a) => [a] -> Int
numberOfIncreases l = length . filter (> 0) $ zipWith (-) (tail l) l

-- 02.12.21
multPair :: (Num a) => (a, a) -> a
multPair = uncurry (*)

data Instruction a
  = Up a
  | Down a
  | Forward a
  deriving (Show)

-- Unfortunately as we cannot go over the water we have to use Ord as well
-- (due to the use of max)
finalPosition :: (Num a, Ord a) => [Instruction a] -> (a, a)
finalPosition = foldr go (0, 0)
  where
    go (Up x) (h, d)      = (h, max 0 $ d - x)
    go (Down x) (h, d)    = (h, d + x)
    go (Forward x) (h, d) = (h + x, d)

day2Task :: (Num a, Ord a) => [Instruction a] -> a
day2Task = multPair . finalPosition
santaInstructions :: (Num a, Ord a) => [Instruction a]
santaInstructions = [
  Forward 5,
  Down 5,
  Forward 8,
  Up 3,
  Down 8,
  Forward 2]


-- 03.12.21
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

addPair :: (Num a) => (a, a) -> a
addPair = uncurry (+)

data Bit = Zero | One
  deriving (Show, Eq)

intToBit :: Integer -> Bit
intToBit 0 = Zero
intToBit 1 = One
intToBit _ = error "Cannot transform non bit Integer to type Bit"

bitToInt :: Bit -> Integer
bitToInt Zero = 0
bitToInt One  = 1

type Binary = [Bit]

binaryInIntToBinary :: [Integer] -> Binary
binaryInIntToBinary = map intToBit

binaryToInt :: Binary -> Integer
binaryToInt = foldl go 0
  where
    --go = uncurry . curry addPair . curry $ mapPair (*2) (bitToInt)
    go acc Zero = 2 * acc
    go acc One  = 1 + 2 * acc

binaryComplement :: Binary -> Binary
binaryComplement = map go
  where
    go Zero = One
    go One  = Zero

transpose :: [[a]] -> [[a]]
transpose []       = []
transpose ([] : _) = []
transpose xs       = map head xs : transpose (map tail xs)

higherOccurencesInRow :: [Bit] -> Bit
higherOccurencesInRow l = if zeros > ones then Zero else One
  where
    (zeros, ones) = foldr go (0, 0) l
    go Zero = mapPair (+1) id
    go One  = mapPair id (+1)

powerConsumption :: [[Integer]] -> Integer
powerConsumption xss = go [] $ reverse $ map binaryInIntToBinary $ transpose xss
  where
    go higherBits [] = gamma * epsilon
      where
        gamma = binaryToInt higherBits
        epsilon = binaryToInt $ binaryComplement higherBits
    go higherBits (xs:xss) = go (higherOccurencesInRow xs : higherBits) xss

diagnosticReport :: [[Integer]]
diagnosticReport = [
  [0,0,1,0,0],
  [1,1,1,1,0],
  [1,0,1,1,0],
  [1,0,1,1,1],
  [1,0,1,0,1],
  [0,1,1,1,1],
  [0,0,1,1,1],
  [1,1,1,0,0],
  [1,0,0,0,0],
  [1,1,0,0,1],
  [0,0,0,1,0],
  [0,1,0,1,0]]
