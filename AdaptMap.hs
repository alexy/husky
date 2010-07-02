
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module AdaptMap where

import Prelude hiding (lookup, foldr)

import Data.Word
import Data.Bits

#if __GLASGOW_HASKELL__
import GHC.Base hiding (foldr)
import GHC.Word
#endif

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

{-# INLINE natFromInt #-}
natFromInt :: Key -> Nat
natFromInt i = fromIntegral i

{-# INLINE intFromNat #-}
intFromNat :: Nat -> Key
intFromNat w = fromIntegral w

{-# INLINE shiftRL #-}
shiftRL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
shiftRL (W# x) (I# i)
  = W# (uncheckedShiftRL# x i)
#else
shiftRL x i   = shiftR x i
#endif

--  Big endian operations  
{-# INLINE maskW #-}
maskW :: Nat -> Nat -> Prefix
maskW i m = intFromNat (i .&. (complement (m-1) `xor` m))

{-# INLINE shorter #-}
shorter :: Mask -> Mask -> Bool
shorter m1 m2 = (natFromInt m1) > (natFromInt m2)

{-# INLINE branchMask #-}
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2 = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

{-# INLINE highestBitMask #-}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))

--  Endian independent bit twiddling

{-# INLINE zero #-}
zero :: Key -> Mask -> Bool
zero i m = (natFromInt i) .&. (natFromInt m) == 0

{-# INLINE nomatch #-}
{-# INLINE   match #-}
nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m = (mask i m) /= p
match   i p m = (mask i m) == p

{-# INLINE zeroN #-}
zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

{-# INLINE mask #-}
mask :: Key -> Mask -> Prefix
mask i m = maskW (natFromInt i) (natFromInt m)

------------------------------------------------------------------------

{-
data IntMap a = Nil
              | Tip {-# UNPACK #-} !Key a
              | Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a)
-}

type Prefix = Int
type Mask   = Int
type Key    = Int

class AdaptIntMap a where

    data IntMap a

    empty     :: IntMap a
    singleton :: Key -> a -> IntMap a
    null      :: IntMap a -> Bool
    size      :: IntMap a -> Int

    lookupN       :: Nat -> IntMap a -> Maybe a
    join          :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a

    insert :: Key -> a -> IntMap a -> IntMap a

    insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
    delete        :: Key -> IntMap a -> IntMap a
    bin           :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
    mapWithKey :: (Key -> a -> a) -> IntMap a -> IntMap a
    unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
    foldr :: (Key -> a -> b -> b) -> b -> IntMap a -> b
    foldr' :: (Key -> a -> b -> b) -> b -> IntMap a -> b

------------------------------------------------------------------------

{-# INLINE unionWith #-}
unionWith :: AdaptIntMap a => (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f m1 m2 = unionWithKey (\_ x y -> f x y) m1 m2

{-# INLINE map #-}
map :: AdaptIntMap a => (a -> a) -> IntMap a -> IntMap a
map f m = mapWithKey (\_ x -> f x) m

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
{-# INLINE lookup #-}
lookup :: AdaptIntMap a => Key -> IntMap a -> Maybe a
lookup k t = case natFromInt k of nk -> lookupN nk t

{-# INLINE member #-}
member :: AdaptIntMap a => Key -> IntMap a -> Bool
member k m = case lookup k m of
      Nothing -> False
      Just _  -> True

{-# INLINE notMember #-}
notMember :: AdaptIntMap a => Key -> IntMap a -> Bool
notMember k m = not $ member k m

{-# INLINE findWithDefault #-}
findWithDefault :: AdaptIntMap a => a -> Key -> IntMap a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x

{-# INLINE insertWith #-}
insertWith :: AdaptIntMap a => (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f k x t = insertWithKey (\_ x' y' -> f x' y') k x t

{-# INLINE unionsWith #-}
unionsWith :: AdaptIntMap a => (a->a->a) -> [IntMap a] -> IntMap a
unionsWith f ts = foldlStrict (unionWith f) empty ts

{-# INLINE foldlStrict #-}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

{-# INLINE foldWithKey #-}
foldWithKey :: AdaptIntMap a => (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey f z t = foldr f z t

{-# INLINE fromList #-}
fromList :: AdaptIntMap a => [(Key,a)] -> IntMap a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t

{-# INLINE toAscList #-}
toAscList :: AdaptIntMap a => IntMap a -> [(Key,a)]
toAscList t
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (\(k,_) -> k >=0) (foldr (\k x xs -> (k,x):xs) [] t) in neg ++ pos

------------------------------------------------------------------------

instance AdaptIntMap Int where

    data IntMap Int
            = NilInt
            | TipInt {-# UNPACK #-} !Key {-# UNPACK #-} !Int
            | BinInt {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask (IntMap Int) (IntMap Int)

    empty         = NilInt

    singleton k x = TipInt k x

    null NilInt   = True
    null _        = False

    size t = case t of
          BinInt _ _ l r -> size l + size r
          TipInt _ _     -> 1
          NilInt         -> 0

    lookupN k t
      = case t of
          BinInt _ m l r
            | zeroN k (natFromInt m) -> lookupN k l
            | otherwise              -> lookupN k r
          TipInt kx x
            | (k == natFromInt kx)   -> Just x
            | otherwise              -> Nothing
          NilInt                     -> Nothing

    bin _ _ l NilInt = l
    bin _ _ NilInt r = r
    bin p m l r      = BinInt p m l r

    join p1 t1 p2 t2
      | zero p1 m = BinInt p m t1 t2
      | otherwise = BinInt p m t2 t1
      where
        m = branchMask p1 p2
        p = mask p1 m

    insertWithKey f k x t
      = case t of
          BinInt p m l r
            | nomatch k p m -> join k (TipInt k x) p t
            | zero k m      -> BinInt p m (insertWithKey f k x l) r
            | otherwise     -> BinInt p m l (insertWithKey f k x r)
          TipInt ky y
            | k==ky         -> TipInt k (f k x y)
            | otherwise     -> join k (TipInt k x) ky t
          NilInt -> TipInt k x

    insert k x t
      = case t of
          BinInt p m l r
            | nomatch k p m -> join k (TipInt k x) p t
            | zero k m      -> BinInt p m (insert k x l) r
            | otherwise     -> BinInt p m l (insert k x r)
          TipInt ky _
            | k==ky         -> TipInt k x
            | otherwise     -> join k (TipInt k x) ky t
          NilInt -> TipInt k x

    delete k t
      = case t of
          BinInt p m l r
            | nomatch k p m -> t
            | zero k m      -> bin p m (delete k l) r
            | otherwise     -> bin p m l (delete k r)
          TipInt ky _
            | k==ky         -> NilInt
            | otherwise     -> t
          NilInt            -> NilInt

    mapWithKey f t
      = case t of
          BinInt p m l r -> BinInt p m (mapWithKey f l) (mapWithKey f r)
          TipInt k x     -> TipInt k (f k x)
          NilInt         -> NilInt

    unionWithKey f t1@(BinInt p1 m1 l1 r1) t2@(BinInt p2 m2 l2 r2)
      | shorter m1 m2  = union1
      | shorter m2 m1  = union2
      | p1 == p2       = BinInt p1 m1 (unionWithKey f l1 l2) (unionWithKey f r1 r2)
      | otherwise      = join p1 t1 p2 t2
      where
        union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
                | zero p2 m1        = BinInt p1 m1 (unionWithKey f l1 t2) r1
                | otherwise         = BinInt p1 m1 l1 (unionWithKey f r1 t2)

        union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
                | zero p1 m2        = BinInt p2 m2 (unionWithKey f t1 l2) r2
                | otherwise         = BinInt p2 m2 l2 (unionWithKey f t1 r2)

    unionWithKey f (TipInt k x) t = insertWithKey f k x t
    unionWithKey f t (TipInt k x) = insertWithKey (\k' x' y' -> f k' y' x') k x t  -- right bias
    unionWithKey _ NilInt t  = t
    unionWithKey _ t NilInt  = t

    foldr f z t
      = case t of
          BinInt 0 m l r | m < 0 -> foldr' f (foldr' f z l) r  -- put negative numbers before.
          BinInt _ _ _ _ -> foldr' f z t
          TipInt k x     -> f k x z
          NilInt         -> z

    foldr' f z t
      = case t of
          BinInt _ _ l r -> foldr' f (foldr' f z r) l
          TipInt k x     -> f k x z
          NilInt         -> z

