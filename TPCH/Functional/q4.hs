module Q4 where

import TPCH.Functional.Schema

import FDB.FDB
import FDB.MoreTypes

q4 targetDate =
  countOrdersByPriority $
    filterQ (orderedWithinThreeMonthsOf targetDate) $
      mapQ rowVal $
        subqFilter orderHasAtLeastOneValidLineItem $
          rows orders

type OrderPriority = String

countOrdersByPriority :: Q Order -> Q (OrderPriority, Int)
countOrdersByPriority q =
  orderByPriority $
    subqMap (\(priority, orders) ->
               do orderCount <- count orders
                  return (priority, orderCount)) $
      groupWithKey $
        mapQ o_orderpriority q
  where orderByPriority = orderBy fst

orderedWithinThreeMonthsOf :: Date -> Order -> Bool
orderedWithinThreeMonthsOf minDate order =
  o_orderdate order >= minDate
  && o_orderdate order < (minDate `plusInterval` Months 3)

orderHasAtLeastOneValidLineItem :: Row Order -> SQ Bool
orderHasAtLeastOneValidLineItem orderRow =
  exists $
    filterQ (\item -> l_commitdate item < l_receiptdate item
                      && l_orderkey item == rowId orderRow)
            (readT lineitems)
