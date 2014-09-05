module Main where

import System.Environment
import Runner

main :: IO ()
main = do
    (src : _ : dst : args) <- getArgs
    --_ <- run
    writeFile dst makeFileContents

makeFileContents :: String
makeFileContents = unlines [
                                "module Main where",
                                "import Test.DocTest",
                                "main :: IO ()",
                                "main = doctest [\"-isrc\", \"src/Runner.hs\"]"
                           ]




