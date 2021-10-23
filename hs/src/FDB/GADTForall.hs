{-# LANGUAGE GADTs #-}
module FDB.GADTForall where

import FDB.Utils ((.:), (<&>))

import Data.Word (Word64, Word16)

import Utils.AbstractGraph as Abstract

-- Table type

newtype Table a = Table String

-- Table operators

findTable :: String -> Table a
findTable = Table

-- Q type

-- Defined using GADT
data Q a where
  Filter   :: (a -> Bool) -> Q a -> Q a
  Map      :: (b -> a) -> Q b -> Q a
  Read     :: Table a -> Q a
  GroupBy  :: (v -> k) -> Q v -> Q (k, Q v)
  MapSQ    :: (v -> SQ b) -> Q v -> Q b
  FilterSQ :: (a -> SQ Bool) -> Q a -> Q a
  OrderBy  :: (Ord b) => (a -> b) -> Q a -> Q a
  Take     :: (Integral n) => n -> Q a -> Q a

-- Q operators

pull :: Q a -> a

pull rec@(Filter p q) = let x = pull q
                    in if p x
                         then x
                         else pull rec

pull (Map f q) = f (pull q)

readT :: Table a -> Q a
readT = Read

mapQ :: (a -> b) -> Q a -> Q b
mapQ = Map

filterQ :: (a -> Bool) -> Q a -> Q a
filterQ = Filter

groupByWithKey :: (Ord k) => (a -> k) -> Q a -> Q (k, Q a)
groupByWithKey = GroupBy

groupByQ :: (Ord k) => (a -> k) -> Q a -> Q (Q a)
groupByQ = mapQ snd .: groupByWithKey

orderBy :: (Ord b) => (a -> b) -> Q a -> Q a
orderBy = OrderBy

takeQ :: (Integral n) => n -> Q a -> Q a
takeQ = Take

emptyQ :: Q a
emptyQ = undefined

-- Relational Q operators

eqFilter :: (Eq b) => (a -> b) -> b -> Q a -> Q a
eqFilter = undefined

equiJoin :: (Eq c) => (a -> c) -> (b -> c) -> Q a -> Q b -> Q (a, b)
equiJoin = undefined

-- Q instances

instance Functor Q where
  fmap = Map

instance Applicative Q where
  pure = undefined
  (<*>) = undefined

instance Monad Q where
  (>>=) = undefined

-- SQ type

data SQ a where
  FetchRow  :: RowRef a -> SQ (Row a)
  StateFold :: Agg a b s -> Q a -> SQ b

-- SQ operators

fetchRow :: RowRef a -> SQ (Row a)
fetchRow = FetchRow

fetchForeign :: RowRef a -> SQ a
fetchForeign ref = fetchRow ref <&> rowVal

-- SQ instances

instance Functor SQ where
  fmap = undefined

instance Applicative SQ where
  pure = undefined
  (<*>) = undefined

instance Monad SQ where
  (>>=) = undefined

-- Mixed Q / SQ operators

subqMap :: (a -> SQ b) -> Q a -> Q b
subqMap = MapSQ

subqFilter :: (a -> SQ Bool) -> Q a -> Q a
subqFilter = FilterSQ

-- Note that with the stream processing approach, we lose the nice property
-- of laziness given by traditional folds
-- We might want to add an explicitly lazy fold operation
-- This operation could be implemented more simply using 'safeHead :: Q a -> SQ (Maybe a)'
exists :: Q a -> SQ Bool
exists q = do c <- count $ takeQ 1 q
              return (c > 0)

mapToQ :: (a -> Q b) -> SQ a -> Q b
mapToQ = undefined

toQ :: SQ a -> Q a
toQ = undefined

-- Aggregations

data Agg a b s = Agg (s -> a -> s) s (s -> b)
type Fold a b = Agg a b b
type Fold1 a = Agg a a a

agg :: Agg a b s -> Q a -> SQ b
agg = StateFold

mapAgg :: Agg b c s -> (a -> b) -> Q a -> SQ c
mapAgg g f q = agg g (mapQ f q)

sumAgg :: (Num a) => Fold1 a
sumAgg = undefined

avgAgg :: (Fractional a) => Agg a a (a, a)
avgAgg = undefined

count :: (Integral n) => Q a -> SQ n
count = undefined

minAgg :: (Ord a) => Fold1 a
minAgg = undefined

-- NatJoin

class NatJoin a b where
  (|><|) :: Q a -> Q b -> Q (a, b)

-- NatJoin based functions

(-|><|->) :: (NatJoin a b) => Q a -> Q b -> Q b
x -|><|-> y = mapQ snd (x |><| y)

-- Can make the type checker loop forever
-- See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableInstances
-- instance (NatJoin a b) => NatJoin b a where
  -- a |><| b = b |><| a

(<-|><|-) :: (NatJoin a b) => Q a -> Q b -> Q a
a <-|><|- b = mapQ fst (a |><| b)

-- Row

type TableRef a = RowRef a

data Row a = Row (RowRef a) a
data RowRef a = RowRef TableId RowId
  deriving Eq

newtype RowId = RowId Word64
  deriving Eq

newtype TableId = TableId Word16
  deriving Eq

-- Row operators

rowVal :: Row a -> a
rowVal (Row _ val) = val

rowRef :: Row a -> RowRef a
rowRef (Row ref _) = ref

asRowId :: RowRef a -> RowId
asRowId = undefined
