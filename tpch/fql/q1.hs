module Q1 where

import MoreTypes
import TPCH
import FDB

q1 = orderBy fst . q1Unordered

q1Unordered shipDaysBefore = subqMap computeAggs (groupByItemState $ itemsShippedBefore maxShipDate)
  where
    maxShipDate = dateFromString "1998-12-01" `minusInterval` Day shipDaysBefore

    computeAggs ((returnflag, linestatus), itemsSubset) = do
      sum_qty        <- sumOf l_quantity
      sum_base_price <- sumOf l_extendedprice
      sum_disc_price <- sumOf disc_price
      sum_charge     <- sumOf charge_price
      avg_qty        <- avgOf l_quantity
      avg_price      <- avgOf l_extendedprice
      avg_disc       <- avgOf l_discount
      count_order    <- count itemsSubset
      return (
          (returnflag, linestatus),
          (sum_qty,
            sum_base_price,
            sum_disc_price,
            sum_charge,
            avg_qty,
            avg_price,
            avg_disc,
            count_order)
        )
      where
        sumOf f = mapAgg sumAgg f itemsSubset
        avgOf f = mapAgg avgAgg f itemsSubset

        disc_price li = (l_extendedprice li) * (1 - (l_discount li))
        charge_price li = (disc_price li) * (1 + l_tax li)

groupByItemState :: Q LineItem -> Q ((Char, Char), Q LineItem)
groupByItemState = groupByWithKey (\li -> (l_returnflag li, l_linestatus li))

itemsShippedBefore :: Date -> Q LineItem
itemsShippedBefore maxDateAllowed = filterQ (\li -> (l_shipdate li) <= maxDateAllowed) (readT lineitems)
