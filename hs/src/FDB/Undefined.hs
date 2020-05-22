module FDB.Undefined where

import Data.Functor.Identity

newtype Table a = Table a

newtype Q a = Q a

newtype SQ a = SQ a

readT :: Table a -> Q a
readT = undefined

rows :: Table a -> Q (Row a)
rows = undefined

values :: Table a -> Q a
values t = mapQ rowVal (rows t)

filterQ :: (a -> Bool) -> Q a -> Q a
filterQ = undefined

groupByQ :: (Ord k) => (a -> k) -> Q a -> Q (Q a)
groupByQ = undefined

groupQ :: (Ord a) => Q a -> Q (Q a)
groupQ = undefined

groupWithKey :: (Ord a) => Q a -> Q (a, Q a)
groupWithKey = undefined

subqMap :: (a -> SQ b) -> Q a -> Q b
subqMap = undefined

data Agg a b s = Agg (s -> a -> s) s (s -> b)
type Fold a b = Agg a b b
type Fold1 a = Agg a a a

mapQ :: (a -> b) -> Q a -> Q b
mapQ = undefined

fetchRow :: RowRef a -> SQ (Row a)
fetchRow = undefined

fetchForeign :: TableRef a -> SQ a
fetchForeign = undefined

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

type TableRef a = RowRef a

data Row a = Row (RowRef a) a
data RowRef a = RowRef TableId RowId

type FDBId = Integer
type RowId = FDBId
type TableId = FDBId

rowRef :: Row a -> RowRef a
rowRef = undefined

rowVal :: Row a -> a
rowVal = undefined

rowId :: Row a -> RowId
rowId = asRowId . rowRef

asRowId :: RowRef a -> RowId
asRowId = undefined

instance Eq (RowRef a) where
  (RowRef lid _) == (RowRef rid _) = (lid == rid)

toQ :: SQ a -> Q a
toQ = undefined

emptyQ :: Q a
emptyQ = undefined
