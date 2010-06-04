module Main where

import qualified Data.ByteString.Lazy as BL
import Codec.Compression.GZip
import qualified Data.Binary as D
import qualified Data.Map as M
import System (getArgs)
import JSON2Graph (Graph)

main :: IO ()
main = do
    args <- getArgs
    bs <- BL.readFile (head args)
    let g = D.decode . decompress $ bs :: Graph
    print (M.size g)
