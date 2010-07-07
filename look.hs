{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import SLP (SLP, toIDL)
import qualified Data.Trie as T
import Data.Trie (Trie)
import qualified Data.Map as M
import Data.Map as M
import Data.List (isInfixOf,isSuffixOf)

-- put this and loadXXX into BinaryGraph:
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
-- TODO do we need ti import ByteString from Char8 or Data.ByteString?
import Data.ByteString.Char8 (pack, ByteString)
import Data.Binary
import Codec.Compression.GZip

loadAnyData :: (Binary a) => FilePath -> IO a
loadAnyData fileName = 
    if ".zip" `isSuffixOf` fileName 
      then do
        bs <- fmap decompress $ L.readFile fileName
        return $! decode bs
      else do
        bs <- decodeFile fileName
        return $! bs
        
showVals :: Maybe SLP -> IO ()
showVals (Just v) = do
            putStr " => "
            print $ toIDL v
showVals Nothing = putStrLn "? Not found"
  
main :: IO ()
main = do
    (dcfile : name : _) <- getArgs
    let !user = pack name
    -- the if surely can be eliminated with the golfing monad:
    S.putStr user
    if ".map" `isInfixOf` dcfile 
      then do
        !dc <- loadAnyData dcfile
        putStrLn $ "Done reading trie dc, size " ++ show (M.size dc)
        showVals (M.lookup user dc)
      else do
        !dc <- loadAnyData dcfile
        putStrLn $ "Done reading map dc, size " ++ show (T.size dc)
        showVals (T.lookup user dc)
        
