module TPCH where

import MoreTypes

type GeneratedKey = Integer

type PartKey = GeneratedKey

data Part = Part {
  p_partkey     :: PartKey,
  p_name        :: String,
  p_mfgr        :: String,
  p_brand       :: String,
  p_type        :: String,
  p_size        :: Int,
  p_container   :: String,
  p_retailprice :: Decimal,
  p_comment     :: String
}

parts :: Table Part
parts = undefined

type SupplierKey = GeneratedKey

data Supplier = Supplier {
  s_suppkey   :: SupplierKey,
  s_name      :: String,
  s_address   :: String,
  s_nationkey :: NationKey,
  s_phone     :: PhoneNumber,
  s_acctbal   :: Decimal,
  s_comment   :: String
}

suppliers :: Table Supplier
suppliers = undefined

data PartSupp = PartSupp {
  ps_partkey    :: PartKey,
  ps_suppkey    :: SupplierKey,
  ps_availqty   :: Int,
  ps_supplycost :: Decimal,
  ps_comment    :: String
}

type CustomerKey = GeneratedKey

data Customer = Customer {
  c_custkey    :: CustomerKey,
  c_name       :: String,
  c_address    :: String,
  c_nationkey  :: NationKey,
  c_phone      :: PhoneNumber,
  c_acctbal    :: Decimal,
  c_mktsegment :: String,
  c_comment    :: String
}

type OrderKey = GeneratedKey

data Order = Order {
  o_orderkey      :: OrderKey,
  o_custkey       :: CustomerKey,
  o_orderstatus   :: Char,
  o_totalprice    :: Decimal,
  o_orderdate     :: Date,
  o_orderpriority :: String,
  o_clerk         :: String,
  o_shippriority  :: Int,
  o_comment       :: String
}

data LineItem = LineItem {
  l_orderkey      :: OrderKey,
  l_partkey       :: PartKey,
  l_suppkey       :: SupplierKey,
  l_linenumber    :: Int,
  l_quantity      :: Decimal,
  l_extendedprice :: Decimal,
  l_discount      :: Decimal,
  l_tax           :: Decimal,
  l_returnflag    :: Char,
  l_linestatus    :: Char,
  l_shipdate      :: Date,
  l_commitdate    :: Date,
  l_receiptdate   :: Date,
  l_shipinstruct  :: String,
  l_shipmode      :: String,
  l_comment       :: String
}

type NationKey = GeneratedKey

data Nation = Nation {
  n_nationkey :: NationKey,
  n_name      :: String,
  n_regionkey :: RegionKey,
  n_comment   :: String
}

type RegionKey = GeneratedKey

data Region = Region {
  r_regionkey :: RegionKey,
  r_name      :: String,
  r_comment   :: String
}
