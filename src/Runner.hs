module Runner where

import Test.DocTest
import System.Directory
import Data.List
import System.FilePath.Posix

generateConfig :: [FilePath] -> [String]
generateConfig = ((:) "-isrc" . dropFileExtensions . notCurrentAndParent . filterHaskellSources) 

-- | Drops file extensions
-- 
-- >>> dropFileExtensions ["foo.bar", "bar.baz", "baz.qux"]
-- ["foo","bar","bar"]

dropFileExtensions :: [FilePath] -> [String]
dropFileExtensions = map dropExtension

-- | Filters out current and parent directories 
--
-- >>> notCurrentAndParent ["wat", ".", "..", "wat"]
-- ["wat","wat","nice"]

notCurrentAndParent :: [FilePath] -> [FilePath]
notCurrentAndParent = filter (`notElem` [".", ".."])

-- | Filters out haskell source files
--
-- >>> filterHaskellSources []
-- []
--
-- >>> filterHaskellSources ["foo.hs", "bar.lhs", "foo.txt", "baz.qux"]
-- ["foo.hs","bar.lhs"]
--
filterHaskellSources :: [FilePath] -> [FilePath]
filterHaskellSources = filter isHaskellSource

isHaskellSource :: FilePath -> Bool
isHaskellSource file = isSuffixOf ".hs" file || isSuffixOf ".lhs" file
