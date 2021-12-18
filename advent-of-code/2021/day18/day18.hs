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
import           Data.Foldable    (toList)
import           Data.List        (find, foldl', sort)
import           Data.List.Unique
import qualified Data.Map         as Map
import           Data.Sequence    hiding (lookup)
import           Data.Strings     (strReplace, strSplit, strSplitAll)
import           System.IO

-- 18.12.21
data Pair a = E a | P (Pair a, Pair a) deriving (Show, Read, Eq)

magnitude :: (Num a) => Pair a -> a
magnitude (E x)             = x
magnitude (P (left, right)) = 3 * magnitude left + 2 * magnitude right

split :: (Num a, Integral a) => Pair a -> (Pair a, Bool)
split (E n) = do
  let (l, m) = divMod n 2
  if n > 10
    then (P (E l, E $ l + m), True)
    else (E n, False)
split (P (left, right)) = do
  let (newLeft, leftChanged) = split left
  let (newRight, rightChanged) = split right
  if leftChanged
    then (P (newLeft, right), True)
    else (P (left, newRight), rightChanged)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

parsePair :: String -> Pair Int
parsePair = read . transformToPair

transformToPair :: String -> String
transformToPair = go . strReplace "]" ")" . strReplace "[" "P ("
  where
    parseDigit :: Char -> Maybe Integer
    parseDigit c =
      toInteger <$> find (\x -> toEnum (x + fromEnum '0') == c) [0 .. 9]
    go [] = []
    go (c : str) =
      case parseDigit c of
        Nothing -> c : go str
        Just _  -> 'E' : ' ' : c : go str

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let pairs = map parsePair $ lines contents
  -- let testSplit = P (P (P (P (E 0, E 7), E 4), P (E 15, P (E 0, E 13))), P (E 1, E 1))
  -- print $ split $ fst $ split testSplit
  print pairs

main :: IO ()
main =
  mainWork "test1.txt"
