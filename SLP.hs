{-# LANGUAGE BangPatterns #-}

module SLP 
  ( SLP
  , toIDL
  ) where

import Data.Binary
import Data.Binary.Get

data P = P {-# UNPACK #-} !Int {-# UNPACK #-} !Double

data SLP
    = Nil
    | Cns {-# UNPACK #-} !P SLP

lenSLP :: SLP -> Int
lenSLP sls = go 0 sls
  where
    go !ct (Cns _ tl) = go (ct+1) tl
    go ct Nil = ct

revSLP :: SLP -> SLP
revSLP sls = go Nil sls
  where
    go !acc (Cns p tl) = go (Cns p acc) tl
    go acc Nil = acc

instance Binary SLP where
    put sls = do
        let go (Cns p tl) = put p >> go tl
            go Nil = return ()
        put (lenSLP sls) >> go sls
    get = do
        len <- get :: Get Int
        let go acc 0 = return $! revSLP acc
            go !acc k = do
                p <- get
                go (Cns p acc) (k-1)
        go Nil len

instance Binary P where
    put (P i f) = put i >> put f
    get = do
        !i <- get
        !f <- get
        return $! P i f

toIDL :: SLP -> [(Int,Double)]
toIDL (Cns (P i f) tl) = (i,f):toIDL tl
toIDL _ = []
