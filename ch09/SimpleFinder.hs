module SimpleFinder where

import           RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

simpleFind' :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind' p path = filter p <$> getRecursiveContents path
