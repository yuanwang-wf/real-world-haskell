# Chapter 9. I/O case study: a library for seaching the filesystem

[Link](http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html)

`getRecursiveContents` is a pretty nice example mix monadic and plain function

```haskell
module RecursiveContents (getRecursiveContents) where

import Control.Monad    (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir                   -- retrieve all files and directories under topdir  monadic code
  let properNames = filter (`notElem` [".", ".."]) names -- use let to bind plain value inside a do
  paths <- forM properNames $ \name -> do                -- monadic code
    let path = topdir </> name                           -- local plain value
    isDirectory <- doesDirectoryExist path               -- monadic local value
    if isDirectory
      then getRecursiveContents path
      else return [path]                                 -- use return to wrap values
  return (concat paths)
```
