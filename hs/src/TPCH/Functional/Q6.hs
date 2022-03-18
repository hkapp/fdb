module TPCH.Functional.Q6 where

import TPCH.Functional.Schema

import FDB.MoreTypes
import FDB.FDB

q6 oldDate tgtDiscount maxQty =
  let
    allItems = readT lineitems

    inDateRange l = (l_shipdate l >= oldDate) && (l_shipdate l < oldDate `plusInterval` Years 1)
    itemsInDateRange = filterQ inDateRange allItems

    discountAroundTarget l = (l_discount l >= tgtDiscount - 0.01)
                              && (l_discount l < tgtDiscount + 0.01)
    itemsWithDiscount = filterQ discountAroundTarget itemsInDateRange

    lessQuantity l = l_quantity l < maxQty
    consideredItems = filterQ lessQuantity itemsWithDiscount

    plannedPrice l = l_extendedprice l * l_discount l
    sumOf f q = mapAgg sumAgg f q
  in
    sumOf plannedPrice consideredItems
