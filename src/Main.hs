module Main where

import System.Environment
import Runner
import System.Directory


main :: IO ()
main = do
    (src : _ : dst : args) <- getArgs
    files <- getDirectoryContents "src" 
    let config = generateConfig files 
    writeFile dst (makeFileContents config)

makeFileContents :: [String] -> String
makeFileContents config = unlines [
                                "module Main where",
                                "import Test.DocTest",
                                "main :: IO ()",
                                "main = doctest " ++ show config
                           ]




