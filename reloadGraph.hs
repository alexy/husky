module Main where

import qualified Data.Map as M
import System (getArgs)
import Graph
import BinaryGraph


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    g <- loadGraph fileName
    print (M.size g)
