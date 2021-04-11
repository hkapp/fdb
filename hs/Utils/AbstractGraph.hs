module Utils.AbstractGraph where

import Utils.List (zipWithIndexRight)
import qualified Utils.List as ListUtils
import Utils.Prelude ((<&>))
import qualified Utils.Prelude as Utils

import Data.Bifunctor (Bifunctor(..))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Semigroup (Semigroup(..))
import qualified Data.Set as Set

-- GRAPH API --

-- type definition

data Graph v e = Graph [v] [(v, e, v)]

-- typeclass instances

instance Bifunctor Graph where
  bimap fv fe = mapGraphTriplets fv (mapEdgeFromTriplet fe)

  second fe = mapEdgeTriplets (mapEdgeFromTriplet fe)

-- Semigroup on graphs represent the union
instance Semigroup (Graph v e) where
  (Graph lv le) <> (Graph rv re) = Graph (lv <> rv) (le <> re)

instance Monoid (Graph v e) where
  mempty = Graph [] []
  mappend = (<>)

-- constructors

graphFromTree :: Tree v e -> Graph v e
graphFromTree (Tree node children) =
  let
    edgeTriplets = [ (node, edge, getNode child) | (edge, child) <- children ]
    subGraphs = [ graphFromTree child | (_, child) <- children ]
    localGraph = Graph [node] edgeTriplets
  in
    localGraph <> (mconcat subGraphs)

getNode :: Tree v e -> v
getNode (Tree node _) = node

singleton :: v -> Graph v e
singleton v = Graph [v] []

-- accessors

vertices :: Graph v e -> [v]
vertices (Graph vs _) = vs

edgeTriplets :: Graph v e -> [(v, e, v)]
edgeTriplets (Graph _ es) = es

-- mappers

mapVertices :: (v -> w) -> Graph v e -> Graph w e
mapVertices = first

mapEdges :: (e -> f) -> Graph v e -> Graph v f
mapEdges = second

mapEdgeTriplets :: ((v, e, v) -> f) -> Graph v e -> Graph v f
mapEdgeTriplets f (Graph vs es) = Graph vs (es <&> (\(s, e, d) -> (s, f (s, e, d), d)))

mapGraphTriplets :: (v1 -> v2) -> ((v1, e1, v1) -> e2) -> Graph v1 e1 -> Graph v2 e2
mapGraphTriplets fv fe (Graph vs es) = Graph (map fv vs) (map fvev es)
  where fvev (s, e, d) = (fv s, fe (s, e, d), fv d)

mapEdgeInTriplet :: (e -> f) -> (v, e, v) -> (v, f, v)
mapEdgeInTriplet f (s, e, d) = (s, f e, d)

mapEdgeFromTriplet :: (e -> f) -> (v, e, v) -> f
mapEdgeFromTriplet f (_, e, _) = f e

assignUniqueIds :: (Ord v) => Graph v e -> (Graph (v, Int) e, Map v Int)
assignUniqueIds g =
  let
    idMap = Map.fromList (zipWithIndexRight (vertices g))
    gWithIds = mapVertices (\v -> (v, idMap ! v)) g
  in
    (gWithIds, idMap)

eliminateDuplicateVertices :: (Ord v) => Graph v e -> Graph v e
eliminateDuplicateVertices (Graph vs es) = Graph (ListUtils.noDuplicates vs) es

-- TREE API --

data Tree v e = Tree v [(e, Tree v e)]

buildTree :: (t -> v) -> (t -> [(e, t)]) -> t -> Tree v e
buildTree toNode children root =
  let
    rootNode = toNode root
    rootChildren = children root
    subtrees = [ (edge, recBuildTree child) | (edge, child) <- rootChildren ]
    recBuildTree = buildTree toNode children
  in
    Tree rootNode subtrees

assignTreeIds :: Tree v e -> Tree (v, Int) e
assignTreeIds root = fst (recAssignIds 0 root)
  where
    recAssignIds :: Int -> Tree v e -> (Tree (v, Int) e, Int)
    recAssignIds id (Tree node children) = (Tree (node, id) childrenWithId, nextId)
      where
        (childrenWithId, nextId) = assignInSequence (id + 1) children
        assignInSequence :: Int -> [(e, Tree v e)] -> ([(e, Tree (v, Int) e)], Int)
        assignInSequence freeId ((edge, thisSubtree) : remEdges) =
          let
            (thisSubtreeWithId, idAfterThisSubtree) = recAssignIds freeId thisSubtree
            (remEdgesWithId, nextFreeId) = assignInSequence idAfterThisSubtree remEdges
          in ((edge, thisSubtreeWithId) : remEdgesWithId, nextFreeId)
        assignInSequence freeId [] = ([], freeId)

-- Accessors

allTreeEdges :: Tree v e -> [(v, e, v)]
allTreeEdges (Tree root children) = rootEdges ++ subtreeEdges
  where
    rootEdges = [ (root, edge, (rootNode subtree)) | (edge, subtree) <- children ]
    subtreeEdges = do
      (_, subtree) <- children
      allTreeEdges subtree

allTreeNodes :: Tree v e -> [v]
allTreeNodes t = (rootNode t) : (subtrees t >>= allTreeNodes)

rootNode :: Tree v e -> v
rootNode (Tree root _) = root

subtrees :: Tree v e -> [Tree v e]
subtrees (Tree _ edges) = map snd edges

subtreeNodes :: Tree v e -> [v]
subtreeNodes = map rootNode . subtrees

-- Tree-wide modifications

mapTreeNodes :: (v -> w) -> Tree v e -> Tree w e
mapTreeNodes f (Tree root edges) = Tree (f root) [ (edge, mapTreeNodes f subtree) | (edge, subtree) <- edges ]
