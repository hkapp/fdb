module FDB where

import Data.Functor.Identity

newtype Table a = Table a

newtype Q a = Q a

newtype SQ a = SQ a

readT :: Table a -> Q a
readT = undefined

filterQ :: (a -> Bool) -> Q a -> Q a
filterQ = undefined

groupByQ :: (a -> k) -> Q a -> Q (Q a)
groupByQ = undefined

subqMap :: (a -> SQ b) -> Q a -> Q b
subqMap = undefined

data Agg a b s = Agg (s -> a -> s) s (s -> b)

mapAgg :: Agg b c s -> (a -> b) -> Q a -> SQ c
mapAgg = undefined

sumAgg :: (Num a) => Agg a a s
sumAgg = undefined

avgAgg :: (Fractional a) => Agg a a s
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
