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

We could use `functor` to combine monadic value and plain function. We could re-write `getRecursiveContents`

```haskell
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
    properNames <- map (dir </> ) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    paths <- forM properNames $ \ path -> do
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then getRecursiveContents path
          else return [path]
    return (concat paths)
```

Once we have `getRecursiveContents`, we could use it to find file.

```haskell
module SimpleFinder where

import           RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)
```

We could re-write `simpleFind` use `<$>`

```haskell
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = filter p <$> getRecursiveContents path
```

`Predicate` represent a very important idea: using function to model the question domain.