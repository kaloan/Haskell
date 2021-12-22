{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Data.List     (foldl')
import           Data.Strings  (strSplit, strSplitAll)
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
dirExists = dirExists' ""
  where
    dirExists' _ dir (File _) = False
    dirExists' acc dir (Directory dot parent contents) =
      let resInDeeper = map (dirExists' (acc ++ parent) dir) contents
       in (dir == "/" && dot == "/") || (dir == (acc ++ dot)) || or resInDeeper

mkdir :: Directory -> FS -> FS
mkdir dir fs =
  let dirList = strSplitAll "/" dir
   in mkdir' dirList fs
  where
    mkdir' [] fs = fs
    mkdir' [name] (Directory dot parent contents) = Directory dot parent $ Directory name dot [] : contents

mainWork :: State -> FS -> IO ()
mainWork state@State {cwd = cwd, fwd = fwd} fs = do
  putStr (cwd ++ "> ")
  inp <- getLine
  let command = words inp
  case command of
    ["pwd"] -> do
      putStrLn cwd
      mainWork State {cwd = cwd, fwd = fwd} fs
    ["cd"] -> do
      mainWork State {cwd = "/", fwd = "/"} fs
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
    ["ls", name] -> do
      let fullname = getFullName name state
      if not $ dirExists fullname fs
        then do
          putStrLn $ "Directory " ++ fullname ++ " does not exist!"
        else do
          print $ getDirectoryContents fullname fs
      mainWork State {cwd = cwd, fwd = fwd} fs
    ["mkdir", name] -> do
      let fullname = getFullName name state
      if dirExists fullname fs
        then do
          putStrLn $ "Directory " ++ fullname ++ " already exists!"
          mainWork State {cwd = cwd, fwd = fwd} fs
        else do
          print $ "Created directory " ++ fullname
          mainWork State {cwd = cwd, fwd = fwd} (mkdir fullname fs)
    xs -> print xs

main :: IO ()
main =
  mainWork (State {cwd = "/", fwd = "/"}) (Directory "/" "/" [Directory "asdf" "/" []])
