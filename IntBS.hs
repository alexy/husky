module IntBS (
  IntBS (..),
  empty,
  insert,
  lookupBS,
  lookupInt
) where
  
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.Trie as T
import Data.Trie (Trie)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Binary
import Control.Monad (liftM3)

data IntBS = IntBS { trieIB :: Trie Int, totalIB :: Int, backIB :: IntMap ByteString }
empty = IntBS T.empty 0 M.empty

instance Binary IntBS where 
  put (IntBS t n m) = put t >> put n >> put m
  get = liftM3 IntBS get get get
  
insert :: IntBS -> ByteString -> (IntBS, Int)
insert dic s = 
  let t = trieIB dic
      v = T.lookup s t in
  case v of
    Just i -> (dic,i)
    _ -> let n  = succ . totalIB $ dic
             t' = T.insert s n t
             m  = backIB dic
             m' = M.insert n s m in
             (IntBS t' n m',n)
             
lookupBS :: IntBS -> ByteString -> Maybe Int
lookupBS dic s =
  let t = trieIB dic in
  T.lookup s t
  
lookupInt :: IntBS -> Int -> Maybe ByteString
lookupInt dic i =
  let m = backIB dic in
  M.lookup i m
    
             