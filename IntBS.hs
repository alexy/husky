module IntBS (
  IntBS (..),
  IntMapBS,
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
import BinaryGraph

type IntMapBS = IntMap ByteString
data IntBS = IntBS {backIB :: IntMapBS, totalIB :: Int, trieIB :: Trie Int}
empty = IntBS M.empty 0 T.empty

instance Binary IntBS where 
  put (IntBS a b c) = put a >> put b >> put c
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
             (IntBS m' n t',n)
             
lookupBS :: IntBS -> ByteString -> Maybe Int
lookupBS dic s =
  let t = trieIB dic in
  T.lookup s t
  
lookupInt :: IntBS -> Int -> Maybe ByteString
lookupInt dic i =
  let m = backIB dic in
  M.lookup i m
  
saveIB ib fileName = saveData (backIB ib) fileName