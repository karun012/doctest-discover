module Runner (
    generateConfig
) where

import Data.List
import System.FilePath.Posix

-- | Generates doctest configuration
--
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"]
-- ["-isrc","foo","bar"]
--
generateConfig :: [FilePath] -> [String]
generateConfig = ((:) "-isrc" . dropFileExtensions . notCurrentAndParent . filterHaskellSources) 

-- | Drops file extensions
-- 
-- >>> dropFileExtensions ["foo.bar", "bar.baz", "baz.qux"]
-- ["foo","bar","baz"]

dropFileExtensions :: [FilePath] -> [String]
dropFileExtensions = map dropExtension

-- | Filters out current and parent directories 
--
-- >>> notCurrentAndParent ["wat", ".", "..", "wat"]
-- ["wat","wat"]

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
