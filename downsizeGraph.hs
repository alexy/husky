module Main where

import qualified Data.IntMap as M
import System (getArgs)
import Graph
import BinaryGraph
import System.IO

-- IntMaqp has no elemAt
-- takeFirst n m = let (k,_) = M.elemAt n m
--                    (res,_) = M.split k m 
--                in res

takeFirst n m = M.fromList . take n . M.toAscList $ m

main :: IO ()
main = do
    args <- getArgs
    let [inFile,outFile,maxElems'] = args
        maxElems = read maxElems' :: Int
    hPutStrLn stderr ("downsizing " ++ inFile ++ " to "
                    ++ show maxElems ++ " elements into "
                    ++ outFile)
    g <- loadData inFile :: IO Graph
    let gSmall = if maxElems >= M.size g then 
                    error ("you want to downsize to more than the actual size, " 
                      ++ (show . M.size $ g) ++ " elements present")
                  else 
                    takeFirst maxElems g
    saveData gSmall outFile