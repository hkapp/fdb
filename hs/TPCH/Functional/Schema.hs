module TPCH.Functional.Schema where

import FDB.MoreTypes
import FDB.FDB

-- Tables


data Part = Part {
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


data Supplier = Supplier {
  s_name      :: String,
  s_address   :: String,
  s_nation    :: Nation,
  s_phone     :: PhoneNumber,
  s_acctbal   :: Decimal,
  s_comment   :: String
}

suppliers :: Table Supplier
suppliers = undefined


data PartSupp = PartSupp {
  ps_partref    :: TableRef Part,
  ps_suppref    :: TableRef Supplier,
  ps_availqty   :: Int,
  ps_supplycost :: Decimal,
  ps_comment    :: String
}

partsupps :: Table PartSupp
partsupps = undefined


data Customer = Customer {
  c_name       :: String,
  c_address    :: String,
  c_nation     :: Nation,
  c_phone      :: PhoneNumber,
  c_acctbal    :: Decimal,
  c_mktsegment :: String,
  c_comment    :: String
}

customers :: Table Customer
customers = undefined


data Order = Order {
  o_custref       :: TableRef Customer,
  o_orderstatus   :: Char,
  o_totalprice    :: Decimal,
  o_orderdate     :: Date,
  o_orderpriority :: String,
  o_clerk         :: String,
  o_shippriority  :: Int,
  o_comment       :: String
}

orders :: Table Order
orders = undefined


data LineItem = LineItem {
  l_orderref      :: TableRef Order,
  l_partref       :: TableRef Part,
  l_suppref       :: TableRef Supplier,
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

lineitems :: Table LineItem
lineitems = findTable "LineItems"


data Nation = France | Germany -- | ...
  deriving Eq
  -- there are 25 Nations in total in TPCH

n_name :: Nation -> String
n_name = undefined

n_region :: Nation -> Region
n_region = undefined

n_comment :: Nation -> String
n_comment = undefined

allNations :: [Nation]
allNations = undefined


data Region = EMEA -- | ...
  deriving Eq
  -- there are 5 Regions in total in TPCH

r_name :: Region -> String
r_name = undefined

r_comment :: Region -> String
r_comment = undefined

allRegions :: [Region]
allRegions = undefined

findRegion :: String -> Region
findRegion = undefined

nationsIn :: Region -> [Nation]
nationsIn = undefined


-- Natural joins

-- instance NatJoin PartSupp Part where
  -- (|><|) = equiJoin ps_partkey p_partkey

-- instance NatJoin PartSupp Supplier where
  -- (|><|) = equiJoin ps_suppkey s_suppkey

-- instance NatJoin Order Customer where
  -- (|><|) = equiJoin o_custkey c_custkey

-- instance NatJoin LineItem Order where
  -- (|><|) = equiJoin l_orderkey o_orderkey

-- instance NatJoin LineItem Part where
  -- (|><|) = equiJoin l_partkey p_partkey

-- instance NatJoin LineItem Supplier where
  -- (|><|) = equiJoin l_suppkey s_suppkey
