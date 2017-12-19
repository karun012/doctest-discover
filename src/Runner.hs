module Runner (
    driver
) where

import Data.List
import Config
import System.FilePath.Posix

-- | Generates doctest driver
--
-- >>> let expected = unlines ["module Main where", "import Test.DocTest", "main :: IO ()", "main = doctest [\"-isrc\",\"foo.hs\",\"bar.hs\"]"]
-- >>> let actual = driver ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- >>> expected == actual
-- True
--
driver :: [FilePath] -> Maybe Config -> String
driver files config = unlines $ ["module Main where", "import Test.DocTest", "main :: IO ()", "main = doctest " ++ (show $ generateConfig files config)]

-- | Generates doctest configuration
--
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- ["-isrc","foo.hs","bar.hs"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]) Nothing)
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-isrc","foo.hs"]
--
-- >>> let config = Just (Config Nothing (Just ["qux"]))
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-iqux","foo.hs","bar.hs"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]) (Just ["qux"]))
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-iqux","foo.hs"]
--
generateConfig :: [FilePath] -> Maybe Config -> [String]
generateConfig files (Just (Config Nothing (Just sourceFolders))) = ((++) (map ("-i"++) sourceFolders) . notCurrentAndParent . filterHaskellSources) files
generateConfig files (Just (Config (Just ignoreList) Nothing)) = ((:) "-isrc" . filter (`notElem` ignoreList) . notCurrentAndParent . filterHaskellSources) files
generateConfig files (Just (Config (Just ignoreList) (Just sourceFolders))) = ((++) (map ("-i"++) sourceFolders) . filter (`notElem` ignoreList) . notCurrentAndParent . filterHaskellSources) files
generateConfig files _ = ((:) "-isrc" . notCurrentAndParent . filterHaskellSources) files

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
