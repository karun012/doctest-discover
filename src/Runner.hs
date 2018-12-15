{-# LANGUAGE
    RecordWildCards
  #-}
module Runner (
    driver
) where

import Data.List
import Data.Maybe
import Config
import System.FilePath.Posix

-- | Generates doctest driver
--
-- >>> putStrLn $ driver ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- module Main where
-- import Test.DocTest
-- main :: IO ()
-- main = doctest
--     [ "-isrc"
--     , "foo.hs"
--     , "bar.hs"
--     ]
-- <BLANKLINE>
--
driver :: [FilePath] -> Maybe Config -> String
driver files config = unlines
    [ "module Main where"
    , "import Test.DocTest"
    , "import Prelude"
    , "main :: IO ()"
    , "main = doctest"
    ] ++ (renderList 1 $ generateConfig files config)
  where
    renderList :: Show a => Int -> [a] -> String
    renderList indent xs =
        let (line:lines) = show <$> xs
            lines' = ("[ " ++ line) : ((", " ++) <$> lines) ++ ["]"]
        in  unlines $ indentOne indent <$> lines'
    indentOne indent = (replicate (4 * indent) ' ' ++)

-- | Generates doctest configuration
--
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] Nothing
-- ["-isrc","foo.hs","bar.hs"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]) Nothing Nothing)
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-isrc","foo.hs"]
--
-- >>> let config = Just (Config Nothing (Just ["qux"]) Nothing)
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-iqux","foo.hs","bar.hs"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]) (Just ["qux"]) Nothing)
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-iqux","foo.hs"]
--
-- >>> let config = Just (Config (Just ["bar.hs"]) (Just ["qux"]) (Just ["-some", "--doctest options"]))
-- >>> generateConfig ["foo.hs", "bar.hs", "baz.qux"] config
-- ["-iqux","foo.hs","-some","--doctest","options"]
--
generateConfig :: [FilePath] -> Maybe Config -> [String]
generateConfig files (Just Config {..})
    =  concat [ maybe ["-isrc"] (map ("-i" ++)) $ sourceFolders
              , nub . filter (`notElem` fromMaybe [] ignore) . notCurrentAndParent . filterHaskellSources $ files
              ]
    ++ maybe [] (concat . map words) doctestOptions
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
