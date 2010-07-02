module Graph (
    AdjList
  , Graph
  , User
  , Day
  , Reps
) where

import IntBS (IntBS)
import IntMap (IntMap)
import qualified AdaptMap as A

type User     = Int
type Reps     = A.IntMap Int
type Graph    = IntMap AdjList
type AdjList  = IntMap Reps
type Day      = Int
      
        
      
