module Invert (invert1,invert2) where

import Graph
-- this is our local strictified IntMap
-- would need to get strict versions of unionsWith, unionWith, union, fromListWith, etc.,
-- for the regular one
import qualified IntMap as M

invert1 :: Graph -> Graph
invert1 g = M.unionsWith (M.unionWith M.union) $ 
            [M.singleton c (M.singleton b (M.singleton a d))
              | (a,m') <- M.toList g, (b,m'') <- M.toList m', (c,d) <- M.toList m'']

invert2 :: Graph -> Graph
invert2 g = M.fromListWith (M.unionWith M.union) $ 
            [(c , M.singleton b (M.singleton a d)) 
              | (a,m') <- M.toList g, (b,m'') <- M.toList m', (c,d) <- M.toList m'']
