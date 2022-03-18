module Test.Q6_v1 where

import FDB.RustFFI

type FDBInt = CUInt32

data LineItem = LineItem {
  -- l_orderref      :: TableRef Order,
  -- l_partref       :: TableRef Part,
  -- l_suppref       :: TableRef Supplier,
  -- l_linenumber    :: Int,
  l_quantity      :: FDBInt, -- Decimal,
  l_extendedprice :: FDBInt, -- Decimal,
  l_discount      :: FDBInt, -- Decimal,
  -- l_tax           :: Decimal,
  -- l_returnflag    :: Char,
  -- l_linestatus    :: Char,
  l_shipdate      :: FDBInt -- Date,
  -- l_commitdate    :: Date,
  -- l_receiptdate   :: Date,
  -- l_shipinstruct  :: String,
  -- l_shipmode      :: String,
  -- l_comment       :: String
}

-- q6 dbCtx oldDate tgtDiscount maxQty =
q6 dbCtx =
  do
    allItems <- readT dbCtx "lineitem"
    itemsInDateRange <- filterQ inDateRange "Test.Q6_v1.inDateRange" allItems
    itemsWithDiscount <- filterQ discountAroundTarget "Test.Q6_v1.discountAroundTarget" itemsInDateRange
    consideredItems <- filterQ lessQuantity "Test.Q6_v1.lessQuantity" itemsWithDiscount
    foldQ sumExpectedPrice "Test.Q6_v1.sumExpectedPrice" zero "Test.Q6_v1.zeroi" consideredItems
    -- sumOf plannedPrice consideredItems
  -- sumOf f q = foldQ (\x y -> x + f y) "abc" zero "Test.Q6_v1.zeroi" q
  -- sumOf f q = foldQ (\x y -> x + f y) fooFoldZero1 q
  -- sumOf f q = mapAgg sumAgg f q
  -- itemsInDateRange = filterQ inDateRange "Test.Q6_v1.inDateRange" allItems
  -- itemsInDateRange = filterQ inDateRange allItems
  -- consideredItems = filterQ lessQuantity itemsWithDiscount
  -- itemsWithDiscount = filterQ discountAroundTarget itemsInDateRange

-- inDateRange oldDate l =
inDateRange l =
  let
    oldDate = 1
  in
    l_shipdate l <= oldDate + 1
    -- (l_shipdate l >= oldDate) && (l_shipdate l < oldDate + 1)
-- inDateRange oldDate l = (l_shipdate l >= oldDate) && (l_shipdate l < oldDate + 1)
-- inDateRange l = (l_shipdate l >= oldDate) && (l_shipdate l < oldDate `plusInterval` Years 1)

-- discountAroundTarget tgtDiscount l =
discountAroundTarget l =
  let
    tgtDiscount = 2
    discountRangeRadius = 1
    -- discountRangeRadius = 0.01
  in
    l_discount l <= tgtDiscount + discountRangeRadius
    -- (l_discount l >= tgtDiscount - discountRangeRadius)
    -- && (l_discount l < tgtDiscount + discountRangeRadius)

-- lessQuantity maxQty l = l_quantity l < maxQty
lessQuantity l =
  let
    maxQty = 3
  in
    l_quantity l <= maxQty
    -- l_quantity l < maxQty

sumExpectedPrice s l =
  let
    expectedPrice = l_extendedprice l + l_discount l
    -- expectedPrice = l_extendedprice l * l_discount l
  in
    s + expectedPrice
-- (\x y -> x + f y) plannedPrice

zero :: FDBInt
zero = fromIntegral zeroi

zeroi :: Int
zeroi = 0
