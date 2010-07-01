module Intern (disintern) where

-- disintern[1..3] are by Daniel Fischer, @dafis
-- he also hacked IntMap into a stricter one:

import qualified IntMap as M
-- we used to import Utils (foldWithKey')
-- but now our IntMap's one is strict enough

-- to disintern to a Trie:
  
disintern1 dic =
 let !ib = backIB dic in
     step !k !v !res = {-# SCC "disintern.step" #-} case ib ! k of
                          !name -> M.insert name v res
 in M.foldWithKey step M.empty

-- also try

disintern2 dic dcaps =
 let !tr = trieIB dic
 in fmap (dcaps !) tr

-- (if there is even a faint possibility that the dictionary knows users 
-- without dcaps, instead of (dcaps !), you'd use 
-- (flip (findWithDefault []) dcaps), possibly doing 
-- Data.Map.filter (not . null) afterwards)

-- A related way to try to disintern to a Data.Map,

disintern3 dic dcaps =
 let !mdic = Data.Map.fromDistinctAscList . Data.Trie.toList $ trieIB dic
 in Data.Map.map (dcaps !) mdic