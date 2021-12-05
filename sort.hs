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
