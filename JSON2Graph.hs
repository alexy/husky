module JSON2Graph (
  json2graph,
  AdjList,
  Graph,
  User,
  Day,
  Reps,
  ) where

import qualified Data.Trie as T
import Text.JSONb
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Ratio as R
import Data.Maybe

  
js = B.pack "{\"9\":{\"jovenatheart\":1},\"10\":{\"beverlyyanga\":1},\"31\":{\"mcshellyshell\":1}}"
jo = case decode js of {Right r -> r; _ -> error "can't parse json"}

type Graph = M.Map User AdjList
type AdjList = M.Map Day Reps      
type User = B.ByteString
type Day = Int
type Reps = M.Map User Int

json2AdjList :: B.ByteString -> AdjList
json2AdjList b = 
  case decode b of
      Right r -> json2adj r
      _ -> error ("can't parse json out of string" ++ B.unpack b)

-- we can convert the result type to Maybe, then maybe use the Maybe monad
-- or we can have a single function parsing with pattern guards

json2adj :: JSON -> AdjList
json2adj (Object o) = M.fromList (zip days reps)
  where lo = T.toList o
        (ks,vs) = unzip lo
        days = map (read . B.unpack) ks
        reps = map json2reps vs
j2adj _ = error "bad adj"

json2reps :: JSON -> Reps 
json2reps (Object o) = M.fromList (zip users nums)
  where lo = T.toList o
        (users,vs) = unzip lo
        nums = map json2num vs
j2reps _ = error "bad reps"        
    
json2num :: JSON -> Int
json2num (Number x) = fromIntegral . R.numerator $ x
json2num _ = error "bad j2num"

json2graph :: [(B.ByteString,B.ByteString)] -> Graph
json2graph = M.fromList . map step
  where step (user,json) = (user,json2AdjList json) 