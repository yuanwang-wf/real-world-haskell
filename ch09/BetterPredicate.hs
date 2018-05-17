module BetterPredicate where

import           Control.Exception (SomeException (..), bracket, handle)
import           Control.Monad     (filterM)
import           Data.Time.Clock
import           RecursiveContents (getRecursiveContents)
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtension)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

type Predicate = FilePath       --path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer
               -> UTCTime
               -> Bool

type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing    _  = -1

-- equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k w x y z = f w x y z == k
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP f g b w x y z = f (g w x y z) b

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 f g h w x y z = f (g w x y z) (h w x y z)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' f g b = liftP2 f g (constP b)

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP =  liftP (==)

lessP, greaterP :: (Ord a) => InfoP a -> a -> InfoP Bool
lessP = liftP (<)
greaterP = liftP (>)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f k _ _ _ = f k

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

myTest :: Predicate
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _              = False

myTest' = andP (greaterP sizeP 131072) (liftPath takeExtension `equalP` ".cpp" )

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = filter p <$> getRecursiveContents path

betterFind' :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
betterFind' p path = filterM p =<< getRecursiveContents path

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\ (SomeException _) -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ (fmap Just . hFileSize)


find :: Predicate -> FilePath -> IO Bool
find p name = do
    perms <- getPermissions name
    size  <- getFileSize name
    modified <- getModificationTime name
    return (p name perms size modified)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p = betterFind' (find p)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\ (SomeException _) -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)
