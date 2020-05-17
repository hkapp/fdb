module FDB where

import Data.Functor.Identity

newtype Table a = Table a

newtype Q a = Q a

newtype SQ a = SQ a

readT :: Table a -> Q a
readT = undefined

filterQ :: (a -> Bool) -> Q a -> Q a
filterQ = undefined

groupByQ :: (Ord k) => (a -> k) -> Q a -> Q (Q a)
groupByQ = undefined

subqMap :: (a -> SQ b) -> Q a -> Q b
subqMap = undefined

data Agg a b s = Agg (s -> a -> s) s (s -> b)
type Fold a b = Agg a b b
type Fold1 a = Agg a a a

mapAgg :: Agg b c s -> (a -> b) -> Q a -> SQ c
mapAgg = undefined

sumAgg :: (Num a) => Fold1 a
sumAgg = undefined

avgAgg :: (Fractional a) => Agg a a (a, a)
avgAgg = undefined

count :: (Integral n) => Q a -> SQ n
count = undefined

instance Functor SQ where
  fmap = undefined

instance Applicative SQ where
  pure = undefined
  (<*>) = undefined

instance Monad SQ where
  (>>=) = undefined

instance Functor Q where
  fmap = undefined

instance Applicative Q where
  pure = undefined
  (<*>) = undefined

instance Monad Q where
  (>>=) = undefined

groupByWithKey :: (Ord k) => (a -> k) -> Q a -> Q (k, Q a)
groupByWithKey = undefined

orderBy :: (Ord b) => (a -> b) -> Q a -> Q a
orderBy = undefined

eqFilter :: (Eq b) => (a -> b) -> b -> Q a -> Q a
eqFilter = undefined

class NatJoin a b where
  (|><|) :: Q a -> Q b -> Q (a, b)

(-|><|->) :: (NatJoin a b) => Q a -> Q b -> Q b
(-|><|->) = undefined

exists :: Q a -> SQ Bool
exists = undefined

subqFilter :: (a -> SQ Bool) -> Q a -> Q a
subqFilter = undefined

minAgg :: (Ord a) => Fold1 a
minAgg = undefined

-- Can make the type checker loop forever
-- See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableInstances
-- instance (NatJoin a b) => NatJoin b a where
  -- a |><| b = b |><| a

equiJoin :: (Eq c) => (a -> c) -> (b -> c) -> Q a -> Q b -> Q (a, b)
equiJoin = undefined

(<-|><|-) :: (NatJoin a b) => Q a -> Q b -> Q a
(<-|><|-) = undefined

mapToQ :: (a -> Q b) -> SQ a -> Q b
mapToQ = undefined
