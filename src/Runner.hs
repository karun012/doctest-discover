module Runner (
    driver
) where

import Data.List
import Config
import System.FilePath.Posix

-- | Generates doctest driver
--
-- >>> let expected = unlines ["module Main where", "import Test.DocTest", "main :: IO ()", "main = doctest [\"-isrc\",\"foo\",\"bar\"]"]
-- >>> let actual = driver ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- >>> expected == actual
-- True
--
driver :: [FilePath] -> Maybe Config -> String
driver files config = unlines $ ["module Main where", "import Test.DocTest", "main :: IO ()", "main = doctest " ++ (show $ generateConfig files config)]

-- | Generates doctest configuration
--
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- ["-isrc","foo","bar"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]))
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-isrc","foo"]
--
generateConfig :: [FilePath] -> Maybe Config -> [String]
generateConfig files config = case config of
                                Just (Config (Just (ignoreList))) -> ((:) "-isrc" . dropFileExtensions . filter (`notElem` ignoreList) . notCurrentAndParent . filterHaskellSources) files
                                _ ->  ((:) "-isrc" . dropFileExtensions . notCurrentAndParent . filterHaskellSources) files


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
