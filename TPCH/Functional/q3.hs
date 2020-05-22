module TPCH.Functional.Q3 where

import TPCH.Functional.Schema

import FDB.FDB
import FDB.MoreTypes

import Data.Ord (Down(..))

-- Customer <- Order <- LineItem

-- groupByWithKey
-- then compute aggregate and porject
-- then order by

q3 :: String -> Date -> Q Q3AggRes
q3 marketSegment shipDate =
  orderBy revenueAndOrderDate $
    subqMap revenuePerGroup $
      groupByWithKey orderKeyDateAndPriority $
        lineItemsOrdersAndCustomersFor marketSegment shipDate

type Q3Row = (LineItem, Order, Customer)
type Q3GroupKey = (RowId, Date, Int)
type Q3AggRes = (RowId, Decimal, Date, Int)
type Q3OrderKey = (Down Decimal, Date)

revenueAndOrderDate :: Q3AggRes -> Q3OrderKey
revenueAndOrderDate (orderKey, revenue, orderDate, shipPriority) =
  (Down revenue, orderDate)

revenuePerGroup :: (Q3GroupKey, Q Q3Row) -> SQ Q3AggRes
revenuePerGroup ((orderKey, orderDate, shipPriority), correspondingRows) = do
  revenue <- mapAgg sumAgg
                    (\(item, _, _) -> l_discounted_price item)
                    correspondingRows
  return (orderKey, revenue, orderDate, shipPriority)

orderKeyDateAndPriority :: Q3Row -> Q3GroupKey
orderKeyDateAndPriority (item, order, customer) =
  (l_orderkey item, o_orderdate order, o_shippriority order)

lineItemsOrdersAndCustomersFor :: String -> Date -> Q Q3Row
lineItemsOrdersAndCustomersFor marketSegment shipDate =
  filterQ3Rows marketSegment shipDate $
    subqMap orderAndCustomerFor (readT lineitems)

filterQ3Rows :: String -> Date -> Q Q3Row -> Q Q3Row
filterQ3Rows marketSegment shipDate =
  filterQ (\(item, order, customer) -> itemShippedAfter shipDate item
                                       && orderedBefore shipDate order
                                       && inMarketSegment marketSegment customer)

orderAndCustomerFor :: LineItem -> SQ Q3Row
orderAndCustomerFor item = do
  order    <- fetchForeign $ l_orderref item
  customer <- fetchForeign $ o_custref order
  return (item, order, customer)

inMarketSegment :: String -> Customer -> Bool
inMarketSegment segment customer = c_mktsegment customer == segment

orderedBefore :: Date -> Order -> Bool
orderedBefore maxDate order = o_orderdate order < maxDate

itemShippedAfter :: Date -> LineItem -> Bool
itemShippedAfter minDate item = l_shipdate item > minDate
