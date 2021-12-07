{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Data.List     (foldl')
import           System.IO

type Directory = String

type PlainText = String

newtype File
  = PlainText PlainText
  deriving (Show, Read)

data FS
  = File File
  | Directory Directory Directory [FS]
  deriving (Show, Read)

data State = State {cwd :: Directory, fwd :: Directory} deriving (Show, Read)

getFullName :: Directory -> State -> Directory
getFullName "." State {cwd = cwd}                = cwd
getFullName ".." State {cwd = cwd, fwd = fwd}    = fwd
getFullName given@('/' : rest) State {cwd = cwd} = given
getFullName given State {cwd = cwd}              = cwd ++ given

getDirectoryContents :: Directory -> FS -> [FS]
getDirectoryContents dir (File _) = []
getDirectoryContents dir (Directory dot parent contents) =
  if dir == dot
    then contents
    else concatMap (getDirectoryContents dir) contents

dirExists :: Directory -> FS -> Bool
dirExists dir (File _) = False
dirExists dir (Directory dot parent contents) =
  let resInDeeper = map (dirExists dir) contents
   in (dir == dot) || or resInDeeper

mainWork :: State -> FS -> IO ()
mainWork state@State {cwd = cwd, fwd = fwd} fs = do
  putStr (cwd ++ "> ")
  inp <- getLine
  let command = words inp
  case command of
    ["pwd"] -> do
      putStrLn cwd
      mainWork State {cwd = cwd, fwd = fwd} fs
    ["cd", name] -> do
      let fullname = getFullName name state
      if not $ dirExists fullname fs
        then do
          putStrLn $ "Directory " ++ fullname ++ " does not exist!"
          mainWork State {cwd = cwd, fwd = fwd} fs
        else do
          putStrLn $ "Changing CWD to: " ++ fullname
          mainWork State {cwd = fullname, fwd = fwd} fs
    ["ls"] -> do
      print $ getDirectoryContents cwd fs
      mainWork State {cwd = cwd, fwd = fwd} fs
    xs -> print xs

main :: IO ()
main =
  mainWork (State {cwd = "/", fwd = "/"}) (Directory "/" "/" [])
