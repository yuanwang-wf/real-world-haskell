module RecursiveContents (getRecursiveContents) where

import           Control.Monad    (forM)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))

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

getRecursiveContents' :: FilePath -> IO [FilePath]
getRecursiveContents' dir = do
    properNames <- map (dir </> ) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    paths <- forM properNames $ \ path -> do
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then getRecursiveContents path
          else return [path]
    return (concat paths)
