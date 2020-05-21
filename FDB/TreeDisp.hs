module FDB.TreeDisp where

import Data.Functor.Identity

-- Table type definition

newtype Table a = TypedT TableMetadata
data TableMetadata = Table String

tableMetadata :: Table a -> TableMetadata
tableMetadata (TypedT t) = t

-- Table operators

tableName :: Table a -> String
tableName (TypedT (Table name)) = name

readT :: Table a -> Q a
readT t = TypedQ (ReadT $ tableMetadata t)

-- Q type definition

newtype Q a = TypedQ QTree

data QTree =
  ReadT TableMetadata
  | Map QTree
  | Filter QTree
  | OrderBy QTree
  | GroupBy QTree
  | Empty
 
qtree :: Q a -> QTree
qtree (TypedQ tree) = tree

-- Q operators

addOp :: (QTree -> QTree) -> Q a -> Q b
addOp op (TypedQ qt) = TypedQ (op qt)

mapQ :: (a -> b) -> Q a -> Q b
mapQ _ = addOp Map

filterQ :: (a -> Bool) -> Q a -> Q a
filterQ _ = addOp Filter

groupByQ :: (Ord k) => (a -> k) -> Q a -> Q (Q a)
groupByQ _ = addOp GroupBy

groupByWithKey :: (Ord k) => (a -> k) -> Q a -> Q (k, Q a)
groupByWithKey _ = addOp GroupBy

orderBy :: (Ord b) => (a -> b) -> Q a -> Q a
orderBy _ = addOp OrderBy

eqFilter :: (Eq b) => (a -> b) -> b -> Q a -> Q a
eqFilter extract val = filterQ (\x -> extract x == val)

emptyQ :: Q a
emptyQ = TypedQ Empty

-- Q instances

instance Functor Q where
  fmap = undefined

instance Applicative Q where
  pure = undefined
  (<*>) = undefined

instance Monad Q where
  (>>=) = undefined

-- SQ type definition

newtype SQ a = SQ a

-- SQ operators

-- SQ instances

instance Functor SQ where
  fmap = undefined

instance Applicative SQ where
  pure = undefined
  (<*>) = undefined

instance Monad SQ where
  (>>=) = undefined

-- Mixed Q and SQ operators

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

mapToQ :: (a -> Q b) -> SQ a -> Q b
mapToQ = undefined

toQ :: SQ a -> Q a
toQ = undefined

-- Row type definition

type TableRef a = RowRef a

newtype Row a = Row a
data RowRef a = RowRef RowId a
type RowId = Integer

rowRef :: Row a -> RowRef a
rowRef = undefined

-- Row operators

fetchRow :: RowRef a -> SQ (Row a)
fetchRow = undefined

fetchForeign :: TableRef a -> SQ a
fetchForeign = undefined

-- Row instances

instance Eq (RowRef a) where
  (RowRef lid _) == (RowRef rid _) = (lid == rid)

-- Relational operators

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
