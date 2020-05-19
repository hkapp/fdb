module TPCH.Functional.Q2 where

import TPCH.Functional.Schema

import FDB.FDB
import FDB.MoreTypes

q2 = undefined


partSuppliersFor :: Row Part -> Q PartSupp
partSuppliersFor part = eqFilter ps_partref (rowRef part) (readT partsupps)


fetchRow :: RowRef a -> SQ (Row a)
fetchRow = undefined

fetchForeign :: TableRef a -> SQ a
fetchForeign = undefined

nationIsIn :: Nation -> Region -> Bool
nationIsIn nation region = n_region nation == region

supplierIsIn :: Region -> Supplier -> Bool
supplierIsIn region supplier = nationIsIn (s_nation supplier) region

supplierIsLocatedInRegion :: Region -> PartSupp -> SQ Bool
supplierIsLocatedInRegion region partSupplier =
  do supplier <- fetchForeign $ ps_suppref partSupplier
     return $ supplierIsIn region supplier

partSuppliersLocatedIn :: Region -> Q PartSupp -> Q PartSupp
partSuppliersLocatedIn region = subqFilter (supplierIsLocatedInRegion region)

lowestPriceInRegion :: Row Part -> Region -> SQ Decimal
lowestPriceInRegion part region =
  let ps = partSuppliersLocatedIn region (partSuppliersFor part)
  in mapAgg minAgg ps_supplycost ps

isCheapestInRegion :: Region -> PartSupp -> SQ Bool
isCheapestInRegion region partSupplier = do
  partRow <- fetchRow $ ps_partref partSupplier
  lowestPrice <- lowestPriceInRegion partRow region
  return (ps_supplycost partSupplier == lowestPrice)

cheapestPartSuppliersIn :: Region -> Q PartSupp
cheapestPartSuppliersIn region = subqFilter (isCheapestInRegion region) (readT partsupps)

suppliersLocatedIn :: Region -> Q Supplier -> Q Supplier
suppliersLocatedIn region = filterQ (supplierIsIn region)

q2AllFields partSize partTypeSuffix regionName =
  let
    correctPart part = correctPartSize part && correctPartType part
    correctPartSize p = p_size p == partSize
    correctPartType p = suffixEquals (p_type p) partTypeSuffix
    suffixEquals str suffix = take (length suffix) (reverse str) == (reverse suffix)

    region = findRegion regionName

    finish partSupplier part
      | (correctPart part) = finish2 partSupplier part
      | otherwise = emptyQ

    finish2 partSupplier part = mapToQ (finish3 part) (fetchForeign $ ps_suppref partSupplier)

    finish3 part supplier =
      let nation = s_nation supplier
      in if nationIsIn nation region
           then return (part, supplier, nation)
           else emptyQ
  in do
    partSupplier <- cheapestPartSuppliersIn region
    let part = fetchForeign $ ps_partref partSupplier
    mapToQ (finish partSupplier) part
