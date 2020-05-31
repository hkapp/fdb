module Utils.List where

import qualified Data.List as List

import qualified Data.Set as Set
import Data.Tuple (swap)

noDuplicates :: (Ord a) => [a] -> [a]
noDuplicates = Set.toList . Set.fromList

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

zipWithIndexRight :: [a] -> [(a, Int)]
zipWithIndexRight = map swap . zipWithIndex

groupByKeyVal :: (Eq k) => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKeyVal key val xs = map transformGroup naiveGroups
  where
    naiveGroups = List.groupBy (\x1 x2 -> (key x1) == (key x2)) xs
    transformGroup thisGroup = (thisGroupKey, thisGroupVals)
      where thisGroupKey = key (head thisGroup)
            thisGroupVals = val <$> thisGroup
