{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import System.IO
import BinaryGraph
import Invert

eprintln s = do hPutStrLn stderr s
                hFlush stderr

main :: IO ()
main = do
  args  <-  getArgs
  let [fromName,toName] = args

  eprintln ("reading graph from file: " ++ fromName
    ++ "\n  saving graph in " ++ toName)

  !original <- loadAnyData fromName
  eprintln "loaded"
  let !inverted = invert1 original
  eprintln "inverted"
  saveAnyData toName inverted
