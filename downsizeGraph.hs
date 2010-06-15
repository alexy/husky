module Main where

import qualified Data.Map as M
import System (getArgs)
import Graph
import BinaryGraph
import qualified Data.Map as M
import System.IO

takeFirst n m = let (k,_) = M.elemAt n m
                    (res,_) = M.split k m 
                in res

main :: IO ()
main = do
    args <- getArgs
    let [inFile,outFile,maxElems'] = args
        maxElems = read maxElems' :: Int
    hPrint stderr ("downsizing " ++ inFile ++ " to " 
                    ++ show maxElems ++ " elements into "
                    ++ outFile)
    g <- loadGraph inFile
    let gSmall = if maxElems >= M.size g then 
                    error ("you want to downsize to more than the actual size, " 
                      ++ (show . M.size $ g) ++ " elements present")
                  else 
                    takeFirst maxElems g
    saveGraph gSmall outFile