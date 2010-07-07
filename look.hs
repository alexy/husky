{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import SLP (SLP, toIDL)
import qualified Data.Trie as T
import Data.Trie (Trie)
import qualified Data.Map as M
import Data.Map as M
import Data.List (isInfixOf)

-- put this and loadXXX into BinaryGraph:
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
-- TODO do we need ti import ByteString from Char8 or Data.ByteString?
import Data.ByteString.Char8 (pack, ByteString)
import Data.Binary
import Codec.Compression.GZip

-- these two look the same,
-- unify with -> IO a?
loadDCapsTrie :: FilePath -> IO (Trie SLP)
loadDCapsTrie fp = do
    bs <- fmap decompress $ L.readFile fp
    return $! decode bs

loadDCapsMap :: FilePath -> IO (Map ByteString SLP)
loadDCapsMap fp = do
    bs <- fmap decompress $ L.readFile fp
    return $! decode bs

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
        !dc <- loadDCapsMap dcfile
        putStrLn $ "Done reading trie dc, size " ++ show (M.size dc)
        showVals (M.lookup user dc)
      else do
        !dc <- loadDCapsTrie dcfile
        putStrLn $ "Done reading map dc, size " ++ show (T.size dc)
        showVals (T.lookup user dc)
        
