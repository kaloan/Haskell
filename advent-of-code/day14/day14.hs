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
import           Data.List        (foldl', sort)
import           Data.List.Unique
import           Data.Sequence    hiding (lookup)
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 14.12.21
polymerise :: (Eq a) => Seq a -> [([a], a)] -> Seq a
polymerise = go Empty
  where
    go :: (Eq a) => Seq a -> Seq a -> [([a], a)] -> Seq a
    go acc Empty _ = acc
    go acc (x :<| Empty) _ = acc |> x
    go acc (x :<| y :<| xs) rules =
      case lookup [x, y] rules of
        Just c  -> go (acc |> x |> c) (y :<| xs) rules
        Nothing -> go (acc |> x) (y :<| xs) rules

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
  let (initPolymer', rules') = strSplit "\n\n" contents
  let initPolymer = fromList initPolymer'
  let rules = map (mapPair id head . strSplit " -> ") (lines rules')
  --print $ initPolymer
  --print $ rules
  let finalPolymer = toList $ foldl' (\polymer _ -> polymerise polymer rules) initPolymer [1 .. 10]
  let counts = occurrences finalPolymer
  let maxDiff = fst (last counts) - fst (head counts)
  print counts
  print maxDiff

main :: IO ()
main =
  mainWork "input.txt"
