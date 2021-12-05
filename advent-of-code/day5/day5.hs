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
import           Data.Strings  (strSplit)
import           System.IO

-- 05.12.21
type Point = (Int, Int)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)
listToPair _ = error "The provided list to listToPair does not have 2 elements"

readInt :: String -> Int
readInt = read

deleteByIndex :: Integer -> [a] -> [a]
deleteByIndex n l
  | n < 0 = error "Can't remove negative index"
  | otherwise = go l n []
  where
    go [] _ _ = []
    go (x : xs) m acc =
      if m == 0 then reverse acc ++ xs else go xs (pred m) (x : acc)

turnLineToPairOfCoords :: String -> (Point, Point)
turnLineToPairOfCoords =
  listToPair
    . map (mapPair readInt readInt . strSplit ",")
    . deleteByIndex 1
    . words

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let pairsOfCoords = map turnLineToPairOfCoords $ lines contents
  print pairsOfCoords
