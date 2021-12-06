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

module Main where

import           Control.Monad
import           Data.Array
import           Data.List           (foldl')
import           Data.Strings        (strSplit, strSplitAll)
import qualified Data.Vector         as IV
import qualified Data.Vector.Mutable as V
import           System.IO

-- 06.12.21

histogram :: (Ord a, Eq a) => [a] -> [(a, Int)]
histogram = go . sort id
  where
    go' x []              = [(x, 1)]
    go' x r@((y, n) : ys) = if x == y then (y, succ n) : ys else (x, 1) : r
    go = foldr go' []

addZerosToHistogram :: [(Int, Int)] -> Int -> [(Int, Int)]
addZerosToHistogram l m = go 0 l
  where
    go :: Int -> [(Int, Int)] -> [(Int, Int)]
    go y []
      | y > m = []
      | otherwise = [(x, 0) | x <- [y .. m]]
    go y ((x, n) : xs)
      | y > m = []
      | x > y = go y $ (y, 0) : (x, n) : xs
      | otherwise = (x, n) : go (succ y) xs

readInt :: String -> Int
readInt = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

-- Considering days starting from 0
maxDaysTillBirth :: Int
maxDaysTillBirth = 8

standardDaysTillBirth :: Int
standardDaysTillBirth = 6

sort :: (Ord b) => (a -> b) -> [a] -> [a]
sort _ [] = []
sort f (x : xs) = smaller ++ [x] ++ larger
  where
    smaller = sort f [y | y <- xs, f y < f x]
    larger = sort f [y | y <- xs, f y >= f x]

mainWork :: FilePath -> Int -> IO ()
mainWork filename days = do
  contents <- readFile filename
  let nums = parseIntList contents
  let histogramBasic = histogram nums
  let fullHistogram = addZerosToHistogram histogramBasic maxDaysTillBirth
  print fullHistogram

main :: IO ()
main =
  mainWork "test.txt" 14
