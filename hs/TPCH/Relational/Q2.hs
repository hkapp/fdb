module TPCH.Relational.Q2 where

import TPCH.Relational.Schema

import FDB.FDB
import FDB.MoreTypes

q2 = undefined

regionsNamed :: String -> Q Region -> Q Region
regionsNamed name = eqFilter r_name name

partSuppliersFor :: PartKey -> Q PartSupp
partSuppliersFor partkey = eqFilter ps_partkey partkey (readT partsupps)

partSuppliersRegions :: Q PartSupp -> Q Region
partSuppliersRegions ps = ps -|><|-> (readT suppliers) -|><|-> (readT nations) -|><|-> (readT regions)

lowestPriceInRegion :: PartKey -> String -> SQ Decimal
lowestPriceInRegion partkey regionName =
  let
    ps = partSuppliersFor partkey
    regions = regionsNamed regionName (partSuppliersRegions ps)
    psInRegion = subqFilter (const $ exists regions) ps
  in
    mapAgg minAgg ps_supplycost psInRegion

q2AllFields partSize partTypeSuffix regionName =
  let
    partsOfInterest :: Q Part
    partsOfInterest = filterQ (\p -> correctPartSize p && correctPartType p) (readT parts)
    correctPartSize p = p_size p == partSize
    correctPartType p = suffixEquals (p_type p) partTypeSuffix
    suffixEquals str suffix = take (length suffix) (reverse str) == (reverse suffix)

    isCheapest ps = do lowestPrice <- lowestPriceInRegion (ps_partkey ps) regionName
                       return $ ps_supplycost ps == lowestPrice
    cheapestPartSuppliers :: Q PartSupp
    cheapestPartSuppliers = subqFilter isCheapest (readT partsupps)

    nationsOfInterest :: Q Nation
    nationsOfInterest = (readT nations) <-|><|- (regionsNamed regionName (readT regions))
  in do
    ps <- cheapestPartSuppliers
    p  <- rowJoin ps partsOfInterest
    s  <- rowJoin ps (readT suppliers)
    n  <- rowJoin s nationsOfInterest
    return (p, s, n)

starJoin :: (NatJoin a b) => Q (a, c) -> Q b -> Q (a, (b, c))
starJoin = undefined

chainJoins :: (NatJoin a b) => Q (c, a) -> Q b -> Q ((c, a), b)
chainJoins = undefined

rowJoin :: (NatJoin a b) => a -> Q b -> Q b
rowJoin = undefined
