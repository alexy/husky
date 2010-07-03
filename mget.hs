{-# LANGUAGE OverloadedStrings #-}

import System (getArgs)
import BinaryGraph (loadAnyData)
import Text.Printf
import qualified Data.Map as M
import Data.Map (Map)
import Data.ByteString.Char8

type DCs = Map ByteString [(Int,Double)]

main :: IO ()
main = do
  dcName:key:_ <- getArgs
  dc <- loadAnyData dcName :: IO DCs
  let v = M.lookup (pack key) dc
  printf "key %s => %s\n" key (show v)  