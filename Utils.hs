module Utils (
  foldWithKey'
 ) where

import Data.List (foldl')
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

-- our strict version of M.foldWithKey
-- the step function obeys the parent's param order
-- gotta rename it for IntMap vs just Map
foldWithKey' :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey' f z m = foldl' step z (IM.toList m)
  where step res (k,v) = f k v res
