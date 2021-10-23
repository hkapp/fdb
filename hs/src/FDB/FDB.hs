module FDB.FDB
  (
    Table,
    Q,
    SQ,

    findTable,
    readT,
    filterQ,
    groupByQ,
    subqMap,

    Agg,
    Fold,
    Fold1,

    mapQ,
    fetchRow,
    fetchForeign,
    mapAgg,
    sumAgg,
    avgAgg,
    count,

    groupByWithKey,
    orderBy,
    eqFilter,
    (-|><|->),

    exists,
    subqFilter,
    minAgg,

    NatJoin(..),
    equiJoin,
    (<-|><|-),

    mapToQ,

    TableRef,

    rowRef,
    asRowId,

    Row,
    RowRef,
    RowId,

    toQ,
    emptyQ
  ) where

-- The currently used implementation for FDB
import FDB.GADTForall as FDBImpl
-- import FDB.Undefined as FDBImpl
