module Main where

import System.Random.TreeRandom
import System.Environment (getArgs)

main    :: IO ()
main =  do [arg] <- getArgs
           print $ (toList (tr (read arg) 2) (read arg)) !! (read arg - 1)
