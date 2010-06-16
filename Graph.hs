module Graph (
    AdjList
  , Graph
  , User
  , Day
  , Reps
) where

import IntBS (IntBS)
import Data.IntMap (IntMap)

type User     = Int
type Reps     = IntMap Int
type Graph    = IntMap AdjList
type AdjList  = IntMap Reps
type Day      = Int
      
        
      