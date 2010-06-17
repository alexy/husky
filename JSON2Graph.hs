module JSON2Graph (
  json2graph
  ) where

import Graph
import qualified IntBS as IB
import IntBS (IntBS)
import qualified Data.Trie as T
import Text.JSONb
import qualified Data.IntMap as M
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Ratio as R
import Data.Maybe
import Data.List (mapAccumL)

  
js = B.pack "{\"9\":{\"jovenatheart\":1},\"10\":{\"beverlyyanga\":1},\"31\":{\"mcshellyshell\":1}}"
jo = case decode js of {Right r -> r; _ -> error "can't parse json"}


json2AdjList :: IntBS -> ByteString -> (IntBS,AdjList)
json2AdjList dic b = 
  case decode b of
      Right r -> json2adj dic r
      _ -> error ("can't parse json out of string" ++ B.unpack b)

-- we can convert the result type to Maybe, then maybe use the Maybe monad
-- or we can have a single function parsing with pattern guards

json2adj :: IntBS -> JSON -> (IntBS,AdjList)
json2adj dic (Object o) = (dic',days')
  where (ks,vs) = unzip . T.toList $ o
        days = map (read . B.unpack) $ ks
        (dic',reps) = mapAccumL json2reps dic vs
        days' = M.fromList (zip days reps)
j2adj _ = error "bad adj"

json2reps :: IntBS -> JSON -> (IntBS,Reps) 
json2reps dic (Object o) = (dic',reps)
  where (users,vs) = unzip . T.toList $ o
        (dic',users') = mapAccumL IB.insert dic users
        nums = map json2num vs
        reps = M.fromList (zip users' nums)
j2reps _ = error "bad reps"        
    
json2num :: JSON -> Int
json2num (Number x) = fromIntegral . R.numerator $ x
json2num _ = error "bad j2num"

json2graph :: IntBS -> [(ByteString,ByteString)] -> (IntBS,Graph)
json2graph dic0 js = (dic2,g)
  where
    (users,adjs)  = unzip js
    (dic1,users') = mapAccumL IB.insert dic0 users
    (dic2,adjs')  = mapAccumL json2AdjList dic1 adjs
    g = M.fromList (zip users' adjs')