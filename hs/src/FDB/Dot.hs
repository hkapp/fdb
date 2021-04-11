{-# LANGUAGE GADTs #-}
module FDB.Dot where

import FDB.GADTForall

import Utils.Dot (DotGraph)
import qualified Utils.Dot as Dot

import Control.Monad.Trans.State as State (State, evalState, get, put)

import Data.Semigroup ((<>))

-- Dot utilities

type DotGraphBuilder = State Int (Dot.Node, DotGraph)

toDotGraph :: Q a -> DotGraph
toDotGraph q = snd $ evalState (buildDotGraph q) 0

buildDotGraph :: Q a -> DotGraphBuilder
buildDotGraph (Filter _ subq)   = addNode "Filter" subq
buildDotGraph (Map _ subq)      = addNode "Map" subq
buildDotGraph (Read tab)        = buildReadLeaf tab
buildDotGraph (GroupBy _ subq)  = addNode "GroupBy" subq
buildDotGraph (MapSQ _ subq)    = addNode "MapSQ" subq
buildDotGraph (FilterSQ _ subq) = addNode "FilterSQ" subq
buildDotGraph (OrderBy _ subq)  = addNode "OrderBy" subq
buildDotGraph (Take _ subq)     = addNode "Take" subq -- TODO add right child, the number of rows to take

addNode :: String -> Q b -> DotGraphBuilder
addNode nodeLabel subq = do
  (subqRoot, subqGraph) <- buildDotGraph subq
  newRoot <- newNode nodeLabel
  let edge = Dot.Edge newRoot subqRoot Dot.emptyConfig
  let localGraph = (Dot.mkGraph [newRoot] [edge])
  let totalGraph = localGraph <> subqGraph
  return $ (newRoot, totalGraph)

newNodeId :: State Int Dot.NodeId
newNodeId = do
  currId <- State.get
  let nextId = currId + 1
  State.put nextId
  return $ show currId

newNode :: String -> State Int Dot.Node
newNode label = do
  nodeId <- newNodeId
  return $ Dot.nodeWithLabel nodeId label

buildReadLeaf :: Table a -> DotGraphBuilder
buildReadLeaf (Table name) = do
  readNode <- newNode "Read"
  tabNode  <- newNode name
  let edge = Dot.Edge readNode tabNode Dot.emptyConfig
  let graph = Dot.mkGraph [readNode, tabNode] [edge]
  return (readNode, graph)

-- type QVertex = String
-- type QEdge = ()

-- toAbstractTree :: Q a -> Abstract.Tree QVertex QEdge
-- -- buildTree :: (t -> v) -> (t -> [(e, t)]) -> t -> Tree v e
-- toAbstractTree = Abstract.buildTree opName (\q -> [((), getSubquery q)])

-- toAbstractTreeWithId :: Q a -> Abstract.Tree (QVertex, Int) QEdge

-- toAbstractGraphWithId :: Q a -> Abstract.Graph (QVertex, Int) QEdge

-- toDotGraph :: Q a -> DotGraph

-- opName :: Q a -> String

-- getSubquery :: Q a -> Q b
