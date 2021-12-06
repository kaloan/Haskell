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

readInt :: String -> Int
readInt = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

-- Considering days starting from 0
maxDaysTillBirth :: Int
maxDaysTillBirth = 8

standardDaysTillBirth :: Int
standardDaysTillBirth = 6

mainWork :: FilePath -> Integer -> IO ()
mainWork filename days = do
  contents <- readFile filename
  let nums = parseIntList contents
  vec <- V.toMutableArray $ IV.fromList nums
  -- vec <- do
  --   v <- V.replicate 0 maxDaysTillBirth
  --   let f [x] = V.modify v (+ 1) x
  --   f (x:xs) <- do
  --     V.modify v (+ 1) x
  --     f xs
  --   f nums

  -- vec <- do
  --   v <- V.replicate 0 maxDaysTillBirth
  --   foldl' (\acc x -> V.modify v (+ 1) x) v nums
  --   -- go nums
  --   --   where
  --   --     go [x] = V.modify v (+1) x
  --   --     go (x:xs) = do
  --   --       V.modify v (+1) x
  --   --       go xs
  --   IV.freeze v
  --print $ show $ V.read vec 0
  print $ IV.freeze vec

main :: IO ()
main = do
  mainWork "test.txt" 14
