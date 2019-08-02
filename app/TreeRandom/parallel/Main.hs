module Main where

import System.Random.TreeRandom
import System.Environment (getArgs)

main    :: IO ()
main =  do args <- getArgs
           print $ ptr (args !! 1) (args !! 2)
