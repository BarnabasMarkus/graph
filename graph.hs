module Graph 
  ( Node
  , Weight
  , Edge
  , Graph (..)
  , emptyGraph
  , nodeNum
  , edgeNum
  , addNode
  , delNode
  , addEdge
  , delEdge
  , bulk
  , nodeExists
  , nodeEdges
  ) where

import Data.Semigroup
import Data.List
import Data.Maybe

type Node = Int
type Weight = Float
type Edge = (Node, Node, Weight)

-- | Directed Graph
data Graph = Graph 
  { nodes :: [Node]
  , edges :: [Edge]
  } deriving Eq

instance Show Graph where
  show (Graph nodes edges) = 
    mconcat [ "G { N:", show nodes, " , E:", show edges, " }"]

-- | Merging 2 Graphs into 1
--   The new graph
--   * Nodes:    Node union of graph1 and graph2
--   * Edges:    in case of conflict the graph1's edges will be overwritten
instance Semigroup Graph where
  (<>) graph (Graph nodes edges) = 
    bulk addEdge edges $ 
    bulk addNode nodes graph

instance Monoid Graph where
  mempty = emptyGraph
  mappend = (<>)

-- | Empty graph
emptyGraph :: Graph
emptyGraph = Graph [] []

-- | Number of nodes
nodeNum :: Graph -> Int
nodeNum graph = length $ nodes graph

-- | Number of edges
edgeNum :: Graph -> Int
edgeNum graph = length $ edges graph

-- | Add new node to graph
addNode :: Node -> Graph -> Graph
addNode node (Graph nodes edges) = 
  Graph nodes' edges
    where nodes' = nub $ node : nodes

-- | Delete node from graph
delNode :: Node -> Graph -> Graph
delNode node (Graph nodes edges) = 
  Graph nodes' edges'
    where nodes' = filter (/= node) nodes
          edges' = [ edge | edge@(na, nb, sc) <- edges
                   , na /= node && nb /= node]

-- | Add new / update existing edge bw 2 nodes
addEdge :: Edge -> Graph -> Graph
addEdge (n1,n2,weight) graph = 
  let graph' = (delEdge n1 n2) . (addNode n2) . (addNode n1) $ graph
      nodes' = nodes graph'
      edges' = (n1, n2, weight) : edges graph'
  in Graph nodes' edges'

-- | Delete edge bw 2 nodes
delEdge :: Node -> Node -> Graph -> Graph
delEdge n1 n2 (Graph nodes edges) = 
  Graph nodes edges' where 
    edges' = [ edge | edge@(na, nb, sc) <- edges, not (na == n1 && nb == n2)]

-- | Bulk operations on graph
bulk :: (a -> Graph -> Graph) -> [a] -> Graph -> Graph
bulk _ [] graph = graph
bulk func (x:xs) graph = bulk func xs $ func x graph

-- | Return True if node is elem of the graph, else False
nodeExists :: Node -> Graph -> Bool
nodeExists node graph = node `elem` (nodes graph)

-- | Return all connections of node
nodeEdges :: Node -> Graph -> Maybe [Edge]
nodeEdges node graph@(Graph nodes edges) =
  if not $ nodeExists node graph
  then Nothing
  else Just [(n1,n2,w) | (n1,n2,w) <- edges, n1 == node]
