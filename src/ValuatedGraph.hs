{- |
  Module      : ValuatedGraph
  Description : A module for valuated directed (by default) graphs
  Copyright   : ???
  Maintainer  : ???
-}
module ValuatedGraph where

import Graph (Graph, Edge)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- | Type for valuated graph
newtype ValuatedGraph v -- ^ Type for vertex
                      a -- ^ Type for edge value
  = ValuatedGraph { neighborsMapWithValuation :: Map v [(v, a)] }
  deriving Show

-- | Equality test on graphs
instance (Eq v, Eq a) => Eq (ValuatedGraph v a) where
  (ValuatedGraph nsMap) == (ValuatedGraph nsMap') =
    ((length ls) == (length ls')) && (and $ zipWith (\(k, ns) (k', ns') -> (k == k') && (ns `hasSameElementsAs` ns')) ls ls')
      where ls  = Map.toList nsMap
            ls' = Map.toList nsMap'

-- | Do both lists have the same elements with the same count ?
hasSameElementsAs :: Eq a => [a] -> [a] -> Bool
l `hasSameElementsAs` l' = (null $ l' \\ l) && (null $ l \\ l')

-- !!WARNING!!
-- Some operations on 'Set k' or 'Map k a' require type 'k' to be an instance of 'Ord'.
-- Hence constraint 'Ord v =>' may be required on some functions.

-- | * Build functions

-- | The empty graph
empty :: ValuatedGraph v a
empty = undefined

-- | Build a graph with input set of vertices and no edge
fromVertices :: Set v -> ValuatedGraph v a
fromVertices _ = undefined

-- | Build a graph with input set of edges with their valuation
-- | Due to input map type, result graph necessarily has no parallel edges/loops
fromValuationMap :: Map (Edge v) a -> ValuatedGraph v a
fromValuationMap _ = undefined

-- | Build an non-valuated version of input graph
toUnvaluated :: ValuatedGraph v a -> Graph v
toUnvaluated _ = undefined

-- | Build an "undirected" (i.e. symmetric) version input graph by adding reverse edges when needed
toUndirected :: ValuatedGraph v a -> ValuatedGraph v a
toUndirected _ = undefined

-- | * Query functions

-- | The set of vertices
vertices :: ValuatedGraph v a -> Set v
vertices = Map.keysSet . neighborsMapWithValuation

-- | The list of edges
edges :: ValuatedGraph v a -> [Edge v]
edges _ = undefined

-- | The list of successors
-- | Return 'Nothing' if input is not an actual vertex
successorsListOf :: ValuatedGraph v a -> v -> Maybe [(v, a)]
_ `successorsListOf` _ = undefined

-- | The list of predecessors
-- | Return 'Nothing' if input is not an actual vertex
predecessorsListOf :: ValuatedGraph v a -> v -> Maybe [(v, a)]
_ `predecessorsListOf` _ = undefined

-- | Valuations for input ends in input graph
-- | Return `Nothing` if at least one end in input is not an actual vertex of graph
-- | Return null set if both ends in input are actual vertices but input is not an actual edge of input graph
valueAt :: ValuatedGraph v a -> Edge v -> Maybe (Set a)
_ `valueAt` _ = undefined

-- | Is the input graph "undirected" (i.e. symmetric for all edges) ?
isUndirected :: ValuatedGraph v a -> Bool
isUndirected _ = undefined

-- | * Vertex operations

-- | Build a new graph from input graph with a new vertex
-- | Return input graph if input is already an actual vertex of graph
(+.) :: ValuatedGraph v a -> v -> ValuatedGraph v a
_ +. _ = undefined

-- | Build a new graph from input graph without input vertex (and all edges linked to it)
-- | Return input graph if input is not an actual vertex of graph
(-.) :: ValuatedGraph v a -> v -> ValuatedGraph v a
_ -. _ = undefined

-- | * Edge operations

-- | Build a new graph from input graph with a new edge and its valuation (add its ends if necessary)
(+|) :: ValuatedGraph v a -> (Edge v, a) -> ValuatedGraph v a
_ +| _ = undefined

-- | Build a new graph from input graph without input edge (does NOT remove ends)
-- | Return input graph if input is not an actual edge of graph
(-|) :: ValuatedGraph v a -> (Edge v, a) -> ValuatedGraph v a
_ -| _ = undefined

-- | Build a new graph from input graph with all edges reversed
-- | Return input graph if undirected
reverse :: ValuatedGraph v a -> ValuatedGraph v a
reverse _ = undefined

-- | Compute a shortest path between input vertices
-- | It is assumed that all valuations are nonnegative so DIJKSTRA algorithm can be considered
-- | Return `Nothing` if at least one end in input is not an actual vertex of graph
-- | Return empty list if both ends in input are actual vertices but there is no path
shortestPath :: ValuatedGraph v a -> (v, v) -> Maybe [(Edge v, a)]
_ `shortestPath` _ = undefined

-- | * DOT representation

-- | Computes the DOT representation string of graph
-- | First parameter is the graph name
toDOTString :: (Ord v, Show v, Show a) => String -> ValuatedGraph v a -> String
toDOTString name g@(ValuatedGraph nsMap) = "digraph " ++ name ++ "\n"
  ++ "  {\n" ++ verticesString ++ edgesString ++ "  }\n"
  where verticesString        = concat $ Map.mapWithKey vertexString vertexNumbering
        vertexString v i      = "    " ++ (show i) ++ " [label = \"" ++ (show v) ++ "\"]\n"
        edgesString           = concat $ Map.mapWithKey (\v ns -> concatMap (edgeString v) ns) nsMap
        edgeString v (n, val) = "    " ++ (show $ vertexNumbering ! v) ++ " -> " ++ (show $ vertexNumbering ! n) ++ " [label = \"" ++ (show val) ++ "\"]\n"
        vertexNumbering       = Map.fromList $ zip (Set.toList $ vertices g) [1..]
        
-- | Writes the DOT representation of a graph into a file
-- | First parameter is the graph name
-- | Output file name is graph name with extension .dot
toDOTFile :: (Ord v, Show v, Show a) => String -> ValuatedGraph v a -> IO ()
toDOTFile name = (writeFile $ name ++ ".dot") . (toDOTString name)

