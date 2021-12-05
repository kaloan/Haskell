{-# LANGUAGE RankNTypes #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import           Control.Monad
import           Data.List     (delete, foldl', sortBy)
import           Data.Maybe    (fromJust)
import           System.IO

--import           Data.Sort  (sortOn)

occurenceMap :: (Eq a) => [a] -> [(a, Integer)]
occurenceMap l = go l []
  where
    go [] acc       = acc
    go (x : xs) acc = go xs $ mapIns x acc
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
  deriving (Show, Read)

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
decode encoded hT = go encoded []
  where
    codeMap = map ((\(enc, val) -> (reverse enc, val)) . invert) $ treeToEncoding hT
    go [] _ = []
    go (bit : bits) nonmatched =
      case lookup searchForMatch codeMap of
        Nothing  -> go bits searchForMatch
        (Just x) -> x : go bits []
      where
        --By reversing the encoded strings we can use constant complexity (:)
        --rather than the linear complexity (++)
        --searchForMath = nonmatched ++ [bit]
        searchForMatch = bit : nonmatched

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

deleteByIndex :: Integer -> [a] -> [a]
deleteByIndex n l
  | n < 0 = error "Can't remove negative index"
  | otherwise = go l n []
  where
    go [] _ _ = []
    go (x : xs) m acc =
      if m == 0 then reverse acc ++ xs else go xs (pred m) (x : acc)

findMinWithIndex :: (Ord b) => [a] -> (a -> b) -> (a, Integer)
findMinWithIndex [] _ = error "findMinWithIndex applied to empty list"
findMinWithIndex l project =
  fst $
    foldl'
      ( \((cMin, cMinInd), cIndex) x ->
          if project x < project cMin
            then ((x, cIndex), succ cIndex)
            else ((cMin, cMinInd), succ cIndex)
      )
      ((head l, 0), 0)
      l

sort :: (Ord b) => (a -> b) -> [a] -> [a]
sort _ [] = []
sort f (x : xs) = smaller ++ [x] ++ larger
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
    -- go :: [BinaryTree a Integer] -> BinaryTree a Integer
    -- go []               = error "Empty string"
    -- go [finalTree]      = finalTree
    -- go (t1 : t2 : rest) = go $ insert weight rest $ spliceTrees t1 t2
    go [finalTree] = finalTree
    go forest = go newForest
      where
        smallest = findMinWithIndex forest weight
        removedSmallest = deleteByIndex (snd smallest) forest
        secondSmallest = findMinWithIndex removedSmallest weight
        removedSecondSmallest = deleteByIndex (snd secondSmallest) removedSmallest
        newTree = spliceTrees (fst smallest) (fst secondSmallest)
        newForest = newTree : removedSecondSmallest

--encodeMap :: (Eq a) => [a] -> [(a, [Bit])]
--encodeMap = treeToEncoding . huffmanTree

encode :: (Eq a) => [a] -> ([Bit], BinaryTree a Integer)
encode l = (concatMap (\x -> fromJust $ lookup x encodingMap) l, hT)
  where
    hT = huffmanTree l
    encodingMap = treeToEncoding hT

readBits :: String -> [Bit]
readBits = map go
  where
    go '0' = Zero
    go '1' = One
    go _   = error "Invalid encoding, containing non bit(0 or 1) characters"

readHuffmanTree :: String -> BinaryTree Char Integer
readHuffmanTree = read

encodeFile :: FilePath -> IO ()
encodeFile filename = do
  contents <- readFile filename
  let (encodedStr, tree) = encode contents
  print $ concatMap show encodedStr
  print tree
  writeFile (filename ++ "-encoded") $ concatMap show encodedStr
  writeFile (filename ++ "-tree") $ show tree

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile encodedFile treeFile = do
  encoded' <- readFile encodedFile
  tree' <- readFile treeFile
  let encoded = readBits encoded'
  let tree = readHuffmanTree tree'
  let decoded = decode encoded tree
  putStrLn decoded

main :: IO ()
main = do
  inp <- getLine
  let op = words inp
  case op of
    ["encode", filename]              -> encodeFile filename
    ["decode", encodedFile, treeFile] -> decodeFile encodedFile treeFile
    _                                 -> error "Wrong syntax!"
  main
