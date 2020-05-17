module Q2 where

import FDB
import TPCH
import MoreTypes

q2 = undefined

regionsNamed :: String -> Q Region -> Q Region
regionsNamed name = eqFilter r_name (const name)

partSuppliersFor :: PartKey -> Q PartSupp
partSuppliersFor partkey = eqFilter ps_partkey (const partkey) (readT partsupps)

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
