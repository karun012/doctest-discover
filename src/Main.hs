module Main where

import System.Environment
import Control.Applicative
import Control.Monad (sequence)
import Data.Maybe
import Runner
import Config
import System.Directory


main :: IO ()
main = do
    (src : _ : dst : args) <- getArgs
    let configFileContents = case args of
                              (configFile : _) -> readFile configFile
                              _ -> return ""
    customConfiguration <- config <$> configFileContents
    let sources = case customConfiguration of
                    Just (Config _ (Just sourceFolders)) -> sourceFolders
                    _ -> ["src"]
    files <- sequence $ map getDirectoryContents sources
    let testDriverFileContents = driver (concat files) customConfiguration
    writeFile dst testDriverFileContents





