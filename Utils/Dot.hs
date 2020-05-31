{-|
 Module      : Dot
 Description : Helpers to print graph to a dot format
-}
module Utils.Dot where

import Utils.AbstractGraph (vertices, edgeTriplets, mapGraphTriplets)
import qualified Utils.AbstractGraph as Abstract
import Utils.Prelude ((<&>))
import Utils.PrettyPrint ((+--+), quoted, emptyLine, formatStringList, indent)

import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Public types

type DotConfig = Map String String
type NodeConfig = DotConfig
type EdgeConfig = DotConfig
type GraphConfig = (NodeConfig, EdgeConfig)

type NodeId = String
data Node = Node NodeId NodeConfig
data Edge = Edge Node Node EdgeConfig

data GraphKind = Digraph
data DotGraph = DotGraph GraphKind String [Node] [Edge] GraphConfig

-- Construction

fromAbstractGraph :: Abstract.Graph Node EdgeConfig -> DotGraph
fromAbstractGraph g =
  DotGraph Digraph defaultGraphName (vertices g) (edgeFromTriplet <$> edgeTriplets g) emptyGraphConfig

edgeFromTriplet :: (Node, EdgeConfig, Node) -> Edge
edgeFromTriplet (src, edgeConf, dest) = Edge src dest edgeConf

fromAnyAbstractGraph :: (v -> Node) -> ((v, e, v) -> EdgeConfig) -> Abstract.Graph v e -> DotGraph
fromAnyAbstractGraph buildNode getEdgeConfig g =
  fromAbstractGraph $ mapGraphTriplets buildNode getEdgeConfig g

addNode :: DotGraph -> Node -> DotGraph
addNode (DotGraph kind name nodes edges config) newNode =
  DotGraph kind name (newNode : nodes) edges config

addEdge :: DotGraph -> Edge -> DotGraph
addEdge (DotGraph kind name nodes edges config) newEdge =
  DotGraph kind name nodes (newEdge : edges) config

-- Pretty printing

prettyPrint :: DotGraph -> String
prettyPrint g = unlines [
  prettyKind (graphKind g) +--+ graphName g +--+ "{",
  indent $ prettyNodes (allNodes g),
  emptyLine,
  indent $ prettyEdges (allEdges g),
  "}"
  ]

prettyKind :: GraphKind -> String
prettyKind Digraph = "digraph"

prettyNodes :: [Node] -> String
prettyNodes nodes = unlines (map prettyNode nodes)

prettyNode :: Node -> String
prettyNode (Node id conf) = id +--+ (prettyConf conf) ++ lineEnding

prettyEdges :: [Edge] -> String
prettyEdges edges = unlines (map prettyEdge edges)

prettyEdge :: Edge -> String
prettyEdge (Edge src dst conf) =
  (nodeId src) +--+ "->" +--+ (nodeId dst) +--+ (prettyConf conf) ++ lineEnding

prettyConf :: DotConfig -> String
prettyConf conf
  | (null conf) = ""
  | otherwise =
      let
        items = Map.toList conf
        dotConfItems = items <&> (\(k, v) -> k ++ "=" ++ show v)
      in
        formatStringList "[" ", " "]" dotConfItems

lineEnding :: String
lineEnding = ";"

-- Graph primitives

emptyDigraph :: DotGraph
emptyDigraph = DotGraph Digraph defaultGraphName [] [] emptyGraphConfig

defaultGraphName :: String
defaultGraphName = "G"

graphKind :: DotGraph -> GraphKind
graphKind (DotGraph kind _ _ _ _) = kind

graphName :: DotGraph -> String
graphName (DotGraph _ name _ _ _) = name

allNodes :: DotGraph -> [Node]
allNodes (DotGraph _ _ nodes _ _) = nodes

allEdges :: DotGraph -> [Edge]
allEdges (DotGraph _ _ _ edges _) = edges

-- Node and Edge primitives

nodeId :: Node -> NodeId
nodeId (Node id _) = id

nodeWithLabel :: NodeId -> String -> Node
nodeWithLabel id label = Node id (labelConfig label)

horizontalRecordNode :: NodeId -> [String] -> Node
horizontalRecordNode id content = Node id (horizontalRecord content)

-- Config primitives

emptyConfig :: DotConfig
emptyConfig = Map.empty

emptyGraphConfig :: GraphConfig
emptyGraphConfig = (emptyConfig, emptyConfig)

labelConfig :: String -> DotConfig
labelConfig label = Map.singleton "label" label

horizontalRecord :: [String] -> NodeConfig
horizontalRecord content =
  let
    label = formatStringList "{" "|" "}" content
    config1 = labelConfig label
    config2 = Map.insert "shape" "record" config1
  in
    config2

data ArrowType = Normal | Dot | ODot | None | Empty | Diamond | EDiamond |
                 BoxArrow | Open | Vee | Inv | InvDot | Tee | InvEmpty | ODiamond |
                 Crow | OBox | HalfOpen

edgeEnd :: ArrowType -> EdgeConfig
edgeEnd arrowType = Map.singleton "arrowhead" (strArrowType arrowType)

edgeBegin :: ArrowType -> EdgeConfig
edgeBegin arrowType = Map.fromList [("arrowtail", strArrowType arrowType),
                                    ("dir", "both")]

strArrowType :: ArrowType -> String
strArrowType arrowType = case arrowType of
  Normal   -> "normal"
  Dot      -> "dot"
  ODot     -> "odot"
  None     -> "none"
  Empty    -> "empty"
  Diamond  -> "diamond"
  EDiamond -> "ediamond"
  BoxArrow -> "box"
  Open     -> "open"
  Vee      -> "vee"
  Inv      -> "inv"
  InvDot   -> "invdot"
  Tee      -> "tee"
  InvEmpty -> "invempty"
  ODiamond -> "odiamond"
  Crow     -> "crow"
  OBox     -> "obox"
  HalfOpen -> "halfopen"

data NodeShape = BoxShape | Polygon | Ellipse | Oval |
                 Circle | Point | Egg | Triangle |
                 Pentagon | Hexagon | Octagon |
                 DoubleCircle  -- + missing
 
nodeShape :: NodeShape -> NodeConfig
nodeShape shape = Map.singleton "shape" (strNodeShape shape)

strNodeShape :: NodeShape -> String
strNodeShape shape = case shape of
  BoxShape     -> "box"
  Polygon      -> "polygon"
  Ellipse      -> "ellipse"
  Oval         -> "oval"
  Circle       -> "circle"
  Point        -> "point"
  Egg          -> "egg"
  Triangle     -> "triangle"
  Pentagon     -> "pentagon"
  Hexagon      -> "hexagon"
  Octagon      -> "octagon"
  DoubleCircle -> "doublecircle"
