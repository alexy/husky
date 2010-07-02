{-# LANGUAGE BangPatterns #-}

module Intern (disintern,disintern2) where

-- disintern[1..3] are by Daniel Fischer, @dafis
-- he also hacked IntMap into a stricter one:

import IntBS
import qualified IntMap as M
import IntMap ((!))
import qualified Data.Trie as T
import SocRun (DCaps)

type DCs = T.Trie [(Int,Double)]
  
-- TODO: Cale suggested using builder for toAscList
-- on #haskell circa 2010-06-22 -- see Utils.hs

-- here M is Data.Map.Map ByteString ...
-- disintern1 dic =
--  let !ib = backIB dic
--      step !k !v !res = {-# SCC "disintern.step" #-} case ib ! k of
--                           !name -> M.insert name v res
--  in
--  IM.foldWithKey step M.empty

-- to disintern to a Trie:

disintern2 :: IntBS -> DCaps -> DCs
disintern2 dic dcaps =
 let !tr = trieIB dic
 in fmap (dcaps !) tr

-- (if there is even a faint possibility that the dictionary knows users 
-- without dcaps, instead of (dcaps !), you'd use 
-- (flip (findWithDefault []) dcaps), possibly doing 
-- Data.Map.filter (not . null) afterwards)

-- A related way to try to disintern to a Data.Map,

-- disintern3 dic dcaps =
--  let !mdic = M.fromDistinctAscList . T.toList $ trieIB dic
--  in M.map (dcaps !) mdic

-- this is not strict enough, the thing explodes
-- how can we ensure M.insert stays strict?
-- might as well disintern into a Trie instead

-- current disinterning into a trie without fmap
disintern :: IntBS -> DCaps -> DCs
disintern dic =
 let !ib = backIB dic
     step !k !v !res = {-# SCC "disintern.step" #-} case ib ! k of
                          !name -> T.insert name v res
 in                          
 M.foldWithKey step T.empty
