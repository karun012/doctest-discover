module Main where

import System.Environment
import Control.Applicative
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
    files <- getDirectoryContents "src" 
    let testDriverFileContents = driver files customConfiguration
    writeFile dst testDriverFileContents





