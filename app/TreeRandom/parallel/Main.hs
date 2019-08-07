module Main where

import System.Random.TreeRandom
import System.Environment (getArgs)
import qualified Data.Array.IArray as A

main    :: IO ()
main =  do [arg] <- getArgs
           --print $ (toList (ptr (read arg) 2) (read arg)) !! (read arg - 1)
           print $ (ptra (read arg) 1) A.! (read arg)
