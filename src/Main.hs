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
    let config = generateConfig files customConfiguration
    writeFile dst (makeFileContents config)

makeFileContents :: [String] -> String
makeFileContents config = unlines [
                                "module Main where",
                                "import Test.DocTest",
                                "main :: IO ()",
                                "main = doctest " ++ show config
                           ]




