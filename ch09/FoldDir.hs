module FoldDir where

import           ControlledVist
import           Data.Char       (toLower)
import           System.FilePath (takeExtension, takeFileName, (</>))

data Iterate seed = Done     { unwrap :: seed}
                  | Skip     { unwrap :: seed}
                  | Continue { unwrap :: seed}
                    deriving (Show)


-- Iterator is an alias for the function that we fold with
-- it takes a seed and an Info value representing a directory entry
-- and returns both a new seed and an instruction for our fold function
type Iterator seed = seed -> Info -> Iterate seed


foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = unwrap <$> fold initSeed path
    where
      -- fold :: a -> FilePath -> IO (Iterate a)
      fold seed subpath = getUsefulContents subpath >>= walk seed

      walk seed (name : names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
            done@(Done _) -> return done      -- If the instruction is Done, fold should cease immediately.
            Skip seed'    -> walk seed' names -- if current Info represents a directory,
                                              -- the fold will not recurse into that directory
            Continue seed'
             | isDirectory info -> do
                next <- fold seed' path'
                case next of
                    done@(Done _) -> return done
                    seed''        -> walk (unwrap seed'') names
             | otherwise -> walk seed' names
      walk seed _ = return (Continue seed)


atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
                  | length paths == 3 = Done paths
                  | isDirectory info && takeFileName path == ".git" = Skip paths
                  | extension `elem` [".jpg", ".png"] = Continue (path : paths)
                  | otherwise = Continue paths
                where extension = map toLower (takeExtension path)
                      path = infoPath info

countDirectories :: Iterator Int
countDirectories count info = Continue (if isDirectory info then count + 1 else count)



