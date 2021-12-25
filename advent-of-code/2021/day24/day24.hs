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
import           Data.Char     (isLetter, toUpper)
import           Data.List     (find, foldl', sort)
import           Data.Map      hiding (map)
import           Data.Maybe    (fromJust)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 24.12.21
type Register = Char

type ModelNumber = [Int]

data Second = Register Register | Num Int deriving (Read, Show)

data Instruction
  = Inp Register
  | Add Register Second
  | Mult Register Second
  | Div Register Second
  | Mod Register Second
  | Eql Register Second
  deriving (Read, Show)

isInp :: Instruction -> Bool
isInp (Inp _) = True
isInp _       = False

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

opToFunc :: Instruction -> (Int -> Int -> Int)
opToFunc (Inp _)    = flip const
opToFunc (Add _ _)  = (+)
opToFunc (Mult _ _) = (*)
opToFunc (Div _ _)  = div
opToFunc (Mod _ _)  = mod
-- opToFunc (Eql _ _)  = boolToInt . (==)
opToFunc (Eql _ _)  = \x y -> boolToInt $ x == y

calculate :: Instruction -> Maybe Int -> Map Register Int -> Map Register Int
calculate = undefined

execute :: [Instruction] -> ModelNumber -> ModelNumber
execute program =
  reverse $ snd $ fromJust $ find fst [go n (False, []) program | n <- [9 .. 1]]

registers :: [Register]
registers = ['x', 'y', 'z', 'w']

initRegisters :: Map Register Int
initRegisters = foldl' (\acc reg -> insert reg 0 acc) empty registers

readArithmetic :: String -> Instruction
readArithmetic = read

parseInstruction :: String -> Instruction
parseInstruction = go . strSplitAll " "
  where
    go ["inp", regStr] = Inp (head regStr)
    go [inst, regStr, secondStr] =
      readArithmetic (titleCase inst ++ " '" ++ regStr ++ "' (" ++ sec ++ ")")
      where
        sec =
          if isLetter (head secondStr)
            then "Register '" ++ secondStr ++ "'"
            else "Num " ++ secondStr

titleCase :: String -> String
titleCase []       = []
titleCase (x : xs) = toUpper x : xs

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let lns = lines contents
  let parsed = map parseInstruction lns
  print $ map (strSplitAll " ") lns
  print parsed

main :: IO ()
main = do
  mainWork "test.txt"
