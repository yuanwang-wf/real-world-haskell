# Chapter 9. I/O case study: a library for seaching the filesystem

[Link](http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html)

`getRecursiveContents` is a pretty nice example mix monadic and plain function

```haskell
module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
```
