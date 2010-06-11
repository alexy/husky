module Graph (
    AdjList
  , Graph
  , User
  , Day
  , Reps
) where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

type Graph = M.Map User AdjList
type AdjList = M.Map Day Reps      
type User = B.ByteString
type Day = Int
type Reps = M.Map User Int
