module ControlledVist where

import           Control.Exception (SomeException (..), bracket, handle)
import           Control.Monad     (forM, liftM)
import           Data.List         (sortBy)
import           Data.Time.Clock
import           Prelude           hiding (traverse)
import           RecursiveContents (getRecursiveContents)
import           System.Directory  (Permissions (..), doesDirectoryExist,
                                    getDirectoryContents, getModificationTime,
                                    getPermissions)
import           System.FilePath   ((</>))
import           System.IO         (IOMode (..), hClose, hFileSize, openFile,
                                    withFile)

data Info = Info {
      infoPath    :: FilePath
    , infoPerms   :: Maybe Permissions
    , infoSize    :: Maybe Integer
    , infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (withFile path ReadMode hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

traverseP :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseP order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    fmap concat $ forM (order contents) $ \ info ->
        if isDirectory info && infoPath info /= path
            then traverseP order (infoPath info)
            else return [info]

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\ (SomeException _) -> return Nothing) (Just `fmap` act )

--maybe there is way to write this point free
getUsefulContents :: FilePath -> IO [String]
getUsefulContents dir = filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

isDirectory :: Info -> Bool
isDirectory = maybe False searchable .infoPerms

sortByName :: [Info] -> [Info]
sortByName = sortBy (\ x y -> compare (infoPath x) (infoPath y))

-- What should you pass to traverse to traverse a directory tree in reverse alphabetic order?
-- traverseP sortByName "."

-- Using id as a control function, traverse id performs a preorder
-- traversal of a tree: it returns a parent directory before
-- its children. Write a control function that makes traverse
-- perform a postorder traversal, in which it returns children
-- before their parent.

-- traverseP reverse "."
