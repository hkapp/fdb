module TPCH.Functional.Q2 where

import TPCH.Functional.Schema

import FDB.FDB
import FDB.MoreTypes

import Data.Ord

q2 partSize partTypeSuffix regionName =
  mapQ fieldsOfInterest $
    orderBy orderByClause $
      filterQ partsOfInterest $
        filterQ mustBeInRegion $
          (cheapestPartSupplierNationIn region)
  where
    region = findRegion regionName
    partsOfInterest (p, _, _) = correctPart partSize partTypeSuffix p
    mustBeInRegion  (_, _, n) = nationIsIn region n

fieldsOfInterest (part, supplier, nation) = (
  s_acctbal supplier,
  s_name supplier,
  n_name nation,
  --p_partref part,
  p_mfgr part,
  s_address supplier,
  s_phone supplier,
  s_comment supplier
  )

orderByClause (part, supplier, nation) = (
  Down $ s_acctbal supplier,
  n_name nation,
  s_name supplier--,
  --p_partref part
  )

correctPart :: Int -> String -> Part -> Bool
correctPart partSize partTypeSuffix p = correctPartSize && correctPartType
  where
    correctPartSize = p_size p == partSize
    correctPartType = suffixEquals (p_type p) partTypeSuffix
    suffixEquals str suffix = take (length suffix) (reverse str) == (reverse suffix)

nationIsIn :: Region -> Nation -> Bool
nationIsIn region nation = n_region nation == region

cheapestPartSupplierNationIn :: Region -> Q (Part, Supplier, Nation)
cheapestPartSupplierNationIn region =
  subqMap getPartAndSupplierAndNation (cheapestPartSuppliersIn region)

getPartAndSupplierAndNation :: PartSupp -> SQ (Part, Supplier, Nation)
getPartAndSupplierAndNation partSupp = do
  part <- fetchForeign $ ps_partref partSupp
  supplier <- fetchForeign $ ps_suppref partSupp
  let nation = s_nation supplier
  return (part, supplier, nation)

-- Subquery

cheapestPartSuppliersIn :: Region -> Q PartSupp
cheapestPartSuppliersIn region = subqFilter (isCheapestInRegion region)
                                            (readT partsupps)

isCheapestInRegion :: Region -> PartSupp -> SQ Bool
isCheapestInRegion region partSupplier = do
  partRow     <- fetchRow $ ps_partref partSupplier
  lowestPrice <- lowestPriceInRegion partRow region
  return (ps_supplycost partSupplier == lowestPrice)

lowestPriceInRegion :: Row Part -> Region -> SQ Decimal
lowestPriceInRegion part region =
  mapAgg minAgg ps_supplycost
         (partSuppliersForPartInRegion part region)

partSuppliersForPartInRegion :: Row Part -> Region -> Q PartSupp
partSuppliersForPartInRegion part region =
  subqFilter (supplierIsLocatedIn region)
             (partSuppliersFor part)

partSuppliersFor :: Row Part -> Q PartSupp
partSuppliersFor part = eqFilter ps_partref (rowRef part) (readT partsupps)

supplierIsLocatedIn :: Region -> PartSupp -> SQ Bool
supplierIsLocatedIn region partSupplier =
  do supplier <- fetchForeign $ ps_suppref partSupplier
     return $ supplierIsIn region supplier

supplierIsIn :: Region -> Supplier -> Bool
supplierIsIn region supplier = nationIsIn region (s_nation supplier)
