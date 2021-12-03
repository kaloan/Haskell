{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# LANGUAGE RankNTypes #-}

import           Data.List  (delete, foldl', sortBy)
import           Data.Maybe (fromJust)
--import           Data.Sort  (sortOn)


occurenceMap :: (Eq a) => [a] -> [(a,Integer)]
occurenceMap l = go l []
  where
    go [] acc     = acc
    go (x:xs) acc = go xs $ mapIns x acc
    mapIns x ll = mapIns' x ll [] False
    mapIns' x [] acc False = (x, 1) : acc
    mapIns' _ [] acc True = acc
    mapIns' x ((y, occ) : ys) acc found =
      if y == x
        then mapIns' x ys ((x, succ occ) : acc) True
        else mapIns' x ys ((y, occ) : acc) found

-- We won't construct empty trees anyway
data BinaryTree a b
  = Leaf a b
  | Node b (BinaryTree a b) (BinaryTree a b)
  deriving (Show)

data Bit = Zero | One
  deriving (Eq)
instance Show Bit where
  show Zero = show 0
  show One  = show 1

treeToEncoding :: BinaryTree a b -> [(a, [Bit])]
treeToEncoding tree = go tree []
  where
    go (Leaf x _) path     = [(x, reverse path)]
    go (Node _ lT rT) path = go lT (Zero : path) ++ go rT (One : path)

invert :: (a, b) -> (b, a)
invert (x, y) = (y, x)

decode :: [Bit] -> BinaryTree a Integer -> [a]
decode encoded hT = go encoded [] $ map invert $ treeToEncoding hT
  where
    go [] _ _ = []
    go (bit : bits) nonmatched codeMap =
      case lookup (bit:nonmatched) codeMap of
        Nothing  -> go bits (bit : nonmatched) codeMap
        (Just x) -> x : go bits [] codeMap

weight :: BinaryTree a b -> b
weight (Leaf _ x)   = x
weight (Node x _ _) = x

spliceTrees :: (Ord b, Num b) => BinaryTree a b -> BinaryTree a b -> BinaryTree a b
spliceTrees fT sT =
  if wFT <= wST
    then Node (wFT + wST) fT sT
    else Node (wFT + wST) sT fT
  where
    wFT = weight fT
    wST = weight sT

deleteByIndex :: [a] -> Integer -> [a]
deleteByIndex l n
  | n < 0 = error "Can't remove negative index"
  | otherwise = go l n
  where
    go [] _     = []
    go (_:xs) m = if m == 0 then xs else go xs $ pred m

findMinWithIndex :: (Ord b) => [a] -> (a -> b) -> (a, Integer)
findMinWithIndex [] _ = error "findMinWithIndex applied to empty list"
findMinWithIndex l project =
  fst $
    foldl'
      (\((cMin, cMinInd), cIndex)  x ->
        if project x < project cMin
          then ((x, cIndex), succ cIndex)
          else ((cMin, cMinInd), succ cIndex))
      ((head l, 0), 0)
      l

sort :: (Ord b) => (a -> b) -> [a] -> [a]
sort _ []     = []
sort f (x:xs) = smaller ++ [x] ++ larger
  where
    smaller = sort f [y | y <- xs, f y < f x]
    larger = sort f [y | y <- xs, f y >= f x]

insert :: (Ord b) => (a -> b) -> [a] -> a -> [a]
insert _ [] x = [x]
insert f (x : xs) y =
  if f x < f y
    then x : insert f xs y
    else y : x : xs

huffmanTree :: (Eq a) => [a] -> BinaryTree a Integer
huffmanTree l = go $ sort weight $ map (uncurry Leaf) $ occurenceMap l
  where
    go :: [BinaryTree a Integer] -> BinaryTree a Integer
    go []           = error "Empty string"
    go [finalTree]  = finalTree
    go (t1:t2:rest) = go $ insert weight rest $ spliceTrees t1 t2
    {-go [finalTree] = finalTree
    go forest      = go newForest
      where
        smallest = findMinWithIndex forest weight
        removedSmallest = deleteByIndex forest (snd smallest)
        secondSmallest = findMinWithIndex removedSmallest weight
        removedSecondSmallest = deleteByIndex removedSmallest (snd secondSmallest)
        newTree = spliceTrees (fst smallest) (fst secondSmallest)
        newForest = newTree : removedSecondSmallest-}

encodeMap :: (Eq a) => [a] -> [(a, [Bit])]
encodeMap = treeToEncoding . huffmanTree

encode :: (Eq a) => [a] -> [Bit]
encode l = concatMap (\x -> fromJust $ lookup x encodingMap) l
  where
    encodingMap = encodeMap l

main :: IO ()
main = do
  let endcodedStr = encode "abra"
  print endcodedStr
