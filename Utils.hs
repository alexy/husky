module Utils (
  foldWithKey',
  Timings,
  getTiming
 ) where

import Data.List (foldl')
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import System.CPUTime
import Debug.Trace

-- our strict version of M.foldWithKey
-- the step function obeys the parent's param order
-- gotta rename it for IntMap vs just Map

foldWithKey' :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey' f z m = foldl' step z (IM.toList m)
  where step res (k,v) = f k v res

-- @Cale http://tunes.org/~nef/logs/haskell/10.06.22
-- Ah, toAscList is written using foldrWithKey anyway,
-- so if your Map is so large that foldrWithKey is problematic, then perhaps that's a problem.

-- toAscList t = builder (foldrWithKey (\k x xs -> (\cons nil -> cons (k,x) xs)) (\cons nil -> nil) t)
-- Then it'd fuse.

-- @kmc:
-- System.CPUTime.getCPUTime

-- @FunctorSalad:
-- system "ps -p $PPID -o etime,cmd"
-- the ,cmd is just there for diagnosis
-- 'system' from System.Process

type Timings = [Int]

getTiming :: Maybe String -> IO Int
getTiming msg = do
  t <- getCPUTime
  -- how do I convert 1e9 to teh type of 1000000000?
  let !timing = let x = fromIntegral t `div` 1000000000 in
                   case msg of
                     Nothing -> x
                     Just s -> trace (s ++ (show x)) x
  return timing
