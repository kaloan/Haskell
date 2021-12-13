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
import           Data.List     (foldl', sort)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 13.12.21
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let (points', folds') = strSplit "\n\n" contents
  let points = map (mapPair readInt readInt . strSplit ",") $ lines points'
  let folds = map (mapPair head readInt . strSplit "=" . (head . tail . tail . words)) (lines folds')
  print points
  print folds

main :: IO ()
main =
  mainWork "test.txt"
