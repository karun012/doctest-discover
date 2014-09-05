module Main where

import System.Environment
import Runner

main :: IO ()
main = do
    (src : _ : dst : args) <- getArgs
    _ <- run
    writeFile dst "main = undefined"




