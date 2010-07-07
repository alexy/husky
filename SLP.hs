{-# LANGUAGE BangPatterns #-}

module SLP
  ( SLP(..)
  , toIDL
  , appSLP
  , singSLP
  , P(..)
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Unsafe.Coerce

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

appSLP :: SLP -> SLP -> SLP
appSLP Nil l = l
appSLP l Nil = l
appSLP (Cns p tl) l = Cns p (go tl)
  where
    go Nil = l
    go (Cns q u) = Cns q (go u)

singSLP :: Int -> Double -> SLP
singSLP i d = Cns (P i d) Nil

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
    put (P i f) = putWord64be (fromIntegral i) >> putWord64be (unsafeCoerce f)
    get = do
        !i <- fmap fromIntegral getWord64be
        !f <- fmap unsafeCoerce getWord64be
        return $ P i f

toIDL :: SLP -> [(Int,Double)]
toIDL (Cns (P i f) tl) = (i,f):toIDL tl
toIDL _ = []
