
import Date

q1 shipDaysBefore = subqMap computeAggs (groupByItemState $ itemsShippedBefore maxShipDate)
  where
    computeAggs itemsSubset = do
      sum_qty <- sumOf l_quantity
      sum_base_price <- sumOf l_extendedprice
      sum_disc_price <- sumOf disc_price
      sum_charge <- sumOf charge_price
      avg_qty <- avgOf l_quantity
      avg_price <- avgOf l_extendedprice
      avg_disc <- avgOf l_discount
      count_order <- count itemsSubset
      return (
        sum_qty,
        sum_base_price,
        sum_disc_price,
        sum_charge,
        avg_qty,
        avg_price,
        avg_disc,
        count_order)

    sumOf f = mapAgg sum f itemsSubset
    avgOf f = mapAgg avg f itemsSubset

    disc_price li = (l_extendedprice li) * (1 - (l_discount li))
    charge_price li = (disc_price li) * (1 + l_tax li)

    maxShipDate = Date.fromString "1998-12-01" `minusInterval` Day shipDaysBefore

groupByItemState :: Q LineItem -> Q (Q LineItem)
groupByItemState = groupBy (\li -> (l_returnflag, l_linestatus))

itemsShippedBefore :: Date -> Q LineItem
itemsShippedBefore maxDateAllowed = filter (\li -> (l_shipdate li) <= maxDateAllowed) (readT lineitems)
