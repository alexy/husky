{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import SLP (SLP, toIDL)
import qualified Data.Trie as T
import Data.Trie (Trie)
import Data.ByteString.Char8 (pack)

-- put this and loadXXX into BinaryGraph:
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Binary
import Codec.Compression.GZip

loadDCapsTrie :: FilePath -> IO (Trie SLP)
loadDCapsTrie fp = do
    bs <- fmap decompress $ L.readFile fp
    return $! decode bs

main :: IO ()
main = do
    (dcfile : name : _) <- getArgs
    let !user = pack name
    !dc <- loadDCapsTrie dcfile
    putStrLn $ "Done reading, size " ++ show (T.size dc)
    case T.lookup user dc of
        Nothing -> putStrLn "Not found"
        Just v -> do
            S.putStr user
            putStr " => "
            print $ toIDL v

