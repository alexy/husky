{-# LANGUAGE OverloadedStrings #-}

import Intern (DCs)
import System (getArgs)
import BinaryGraph (loadAnyData)
import Text.Printf
import qualified Data.Trie as T
import Data.ByteString.Char8


main :: IO ()
main = do
  (!dcName) : (!key):_ <- getArgs
  !dc <- loadAnyData dcName :: IO DCs
  let !v = T.lookup (pack key) dc
  printf "key %s => %s\n" key (show v)  
